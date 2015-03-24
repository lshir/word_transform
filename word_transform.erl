-module(word_transform).
-author('Shir Levkowitz <shir@lhaim.com>').
-compile(export_all).

%%
%% DESCRIPTION 
%% 
%% This performs a tree search with the branches as dict keys and the status of that branch as the value.
%% We can't filter the branches (paths) by some distance because we don't have a guarantee of convexity
%% (i.e. two words that "look" similar under most metrics could be far apart)
%%
%% Words for dictionary sampled from http://www.morewords.com
%%



%%
%% EXTERNAL
%%

%% Will start the search, which keeps track of matches and the success of the search
%%
%% StartWord - A word to begin our search
%% TargetWord - The word we are looking for
%% Dictionary - A list of words that may be used. Must include the TargetWord.
transform(StartWord, TargetWord, Dictionary) ->
  PathDict = dict:new(),
  DictionaryComplete = case lists:member(TargetWord, Dictionary) of true -> Dictionary; false -> [TargetWord | Dictionary] end -- [StartWord],
  Result = search(TargetWord, [], dict:store([StartWord], live, PathDict), DictionaryComplete),

  Message = case Result of [] -> "No result was found!~n";
    _ -> string:join(lists:reverse(Result), "\n")
  end,
  io:format(Message ++ "\n"),
  lists:reverse(Result).


%%
%% INTERNAL 
%% 

%% Executes the search
%%
%% Target - The word we are looking for 
%% Match - The best match so far
%% Paths - A dictionary of paths. Key is the list of words, value is the status, which could be either 'live' (continue looking here), 'dead' (don't continue), or Pid (actively looking here)
%% Dictionary - A list of words that may be used.

% Exit clauses
search(_Target, Match, [],  _Dictionary) -> % ran out of paths to search
  Match;
search(_Target, [], _Paths,  []) -> % ran out of dictionary to search
  failure;

% Entry / loop clause
search(Target, Match, Paths, Dictionary) ->
  % Check for early exit if all paths are dead
  NonDeadPaths = dict:filter(fun(_Key, Status) -> Status =/= dead end, Paths),

  % Spawn search processes
  Whoami = self(),
  PathsSpawned = dict:map(fun(Path, Status) ->
    case Status of live ->
        spawn(fun() -> Whoami ! {self(), search_neighbors(Path, Target, Dictionary)} end);
      _ -> Status
    end      
  end, Paths),

  receive
    {_From, {dead, Path}} ->
      NewPaths = dict:store(Path, dead, PathsSpawned),
      NewMatch = Match,
      NewDictionary = Dictionary;
    {_From, {found, MatchContender, Examined}} ->
      NewMatch = if
        length(Match) < length(MatchContender) , Match =/= [] -> Match;
        true -> MatchContender
      end,
      io:format("Match was ~p to ~p~n", [length(Match), length(MatchContender)]),
      NewDictionary = Dictionary -- Examined,
      MatchBase = tl(MatchContender), % this is the base of the match, which we must mark as dead so as to avoid revisiting

      % we remove paths that are longer than the best match
      NewPaths = 
        dict:map(fun(Key, Status) ->
          NewStatus = case length(Key) >= length(NewMatch) of
            true -> dead;
            false -> Status
          end,
          NewStatus
        end, dict:store(MatchBase, dead, PathsSpawned));
    {_From, {notfound, PathBase, PathContenders}} -> 
      NewMatch = Match,
      NewPaths =
        combine_path_lists( 
          % old portion
          dict:store(PathBase, dead, PathsSpawned), 
          % new paths
          PathContenders
        ),
      NewDictionary = Dictionary -- lists:map(fun(NewPath) -> hd(NewPath) end, dict:fetch_keys(PathContenders)) % remove visited members of dictionary
    after 0 -> 
      NewPaths = Paths,
      NewMatch = Match,
      NewDictionary = Dictionary
  end,


  % If all the paths are dead, we pass an empty pathset to prompt function return
  NewPathsNonDead = case dict:is_empty(NonDeadPaths) of true -> [];
    false -> NewPaths
  end,
  search(Target, NewMatch, NewPathsNonDead, NewDictionary).


%% Simple helper function to combine two sets of paths. Where their keys overlap the first one is used.
%%
%% PathsFavored - A preferred proplist of paths
%% PathsSecondary - A proplist of paths
combine_path_lists(PathsFavored, PathsSecondary) ->
  dict:merge(fun(_K, V1, _V2) -> V1 end, PathsFavored, PathsSecondary).


%% Reads in the file as a path and returns the words in a list.
%% @todo for the success of our search, we need to check that the target is in the dictionary
%%
%% File - A filename to read in
read_dictionary(File) ->
  {ok, Binary} = file:read_file(File),
  string:tokens(binary_to_list(Binary), "\n").


%% The assumption here is that our alphabet is lowercase a-z, but we want this to be flexible enough to handle other alphabets.
define_alphabet() ->
  lists:map(fun(X) -> [X] end, lists:seq(97, 122)).


%% Generates candidates set from a current word and a dictionary
%%
%% CurrentWord - A word
%% Dictionary - A set of words to explore
find_neighbors(CurrentWord, Dictionary) -> 
  Candidates = generate_candidates(CurrentWord),
  lists:filter(fun(Word) -> lists:member(Word, Dictionary) end, Candidates).


%% Wraps find_neighbors to test if we have a match, dead, or a new set of candidates
%%
%% CurrentPath - The list of words in the current path, newest at the head
%% Target - The word we are searching for
%% Dictionary - List of words to explore
search_neighbors(CurrentPath, Target, Dictionary) ->
  CurrentWord = hd(CurrentPath), % could push or pop here, just need to be consistent
  Neighbors = find_neighbors(CurrentWord, Dictionary),
  case lists:member(Target, Neighbors) of 
    true ->
      io:format("Found length ~p~n", [length(CurrentPath) + 1]),
      {found, [Target | CurrentPath], Neighbors};
    false when Neighbors =:= [] -> 
      {dead, CurrentPath};
    false ->
      NewPaths = dict:from_list([{[H] ++ T, live} || H <- Neighbors, T <- [CurrentPath]]),
      {notfound, CurrentPath, NewPaths}
  end.


%% Generates a set of all words that could possibly be reached from the current one (without regard for the dictionary)
%% Loops over each letter in the word, does a Cartesian expansion with all possible substitutions
%%
%% CurrentWord - A word to expand
generate_candidates(CurrentWord) ->
  lists:foldl(fun(Ind, AccIn) -> 
    % Break up CurrentWord into Head (before this letter), CurrentLetter, and Tail
    {Head, CurrentTail} = lists:split(Ind, CurrentWord),
    [CurrentLetter | Tail] = CurrentTail,
    % Exclude the current letter
    Alphabet = define_alphabet() -- [[CurrentLetter]],
    % Recombine in Cartesian expansion
    lists:append([AccIn, [Head ++ NewSub ++ Tail || NewSub <- Alphabet]])
  end, [], lists:seq(0,length(CurrentWord)-1)).


%%
%% TESTS
%%
tests() ->
  % generate_candidates
  ["ax","bx","cx","dx","ex","fx","gx","hx","ix","jx","kx",
   "lx","mx","nx","ox","px","qx","rx","sx","tx","ux","vx","wx",
   "yx","zx","xa","xb","xc","xd","xe","xf","xg","xh","xi","xj",
   "xk","xl","xm","xn","xo","xp","xq","xr","xs","xt","xu","xv",
   "xw","xy","xz"] = generate_candidates("xx"),

  % find_neighbors
  Neighbors = lists:sort(["bbcde","abcdf"]),
  Neighbors = lists:sort(find_neighbors("abcde", ["abcdf", "absss", "sddef", "bbcde"])),

  % search_neighbors
  {found,["absde","abcde","abcdx"], ["absde"]} = search_neighbors(["abcde", "abcdx"], "absde", ["absde", "abbbb", "absdf"]),
  {dead,["abcde","abcdx"]} = search_neighbors(["abcde", "abcdx"], "absde", ["abbbb", "absdf"]),
  {notfound,["abcde", "abcdx"], ResDict} = search_neighbors(["abcde", "abcdx"], "absde", ["abqde","abdde", "abbbb", "absdf"]),
  [{["abqde","abcde","abcdx"], live}, {["abdde","abcde","abcdx"], live}] = dict:to_list(ResDict),

  % combine_path_lists
  [{x,50},{y,60},{z,0}] = dict:to_list(combine_path_lists(dict:from_list([{x, 50}, {y, 60}]), dict:from_list([{y, 0}, {z, 0}]))),

  % search
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],dict:from_list([{["bbcde","abcde"], live}]),["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],dict:from_list([{["bbcde","abcde"], live}, {["abcdf","abcde"], live}]),["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbdf","bbbde","bbcde","abcde"] =
    search( "bbbdf", [],dict:from_list([{["bbcde","abcde"], live}, {["abcdf","abcde"], live}]),["bbbde", "bbcce", "abbde", "bqddd", "bbbdf"] ),
  [] = search( "abccf", [],dict:from_list([{["bbcde","abcde"], live}]),["abcdd", "abbcc", "abccc", "abcce", "bbcde", "bacde", "aacde"] ),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], dict:from_list([{["abcde"], live}]), ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abbbe", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  [] = search( "abbbb", [], dict:from_list([{["abcde"], live}]), ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], dict:from_list([{["abcde"], live}]), ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbeb"] = search( "abbbb", [], dict:from_list([{["abcde"], live}, {["bbbde"], live}, {["abbeb"], live}]), ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),

  % transform
  DictionaryMinimal = word_transform:read_dictionary("dictionary_minimal.txt"),
  [] = transform("barye", "bayou", DictionaryMinimal),
  14 = length(transform("burry", "rings", DictionaryMinimal)),


  ok.