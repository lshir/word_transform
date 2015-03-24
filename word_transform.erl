-module(word_transform).
-author('Shir Levkowitz <shir@lhaim.com>').
-compile(export_all).

%%
%% EXTERNAL
%%

%% Will start the search, which keeps track of matches and the success of the search
%%
%% StartWord - A word to begin our search
%% TargetWord - The word we are looking for
%% Dictionary - A list of words that may be used. Must include the TargetWord.
transform(StartWord, TargetWord, Dictionary) ->
  search(TargetWord, [], [{[StartWord], live}], Dictionary)

  % @todo - format output into pretty
  .


%%
%% INTERNAL 
%% 

%% Executes the search
%%
%% Target - The word we are looking for 
%% Match - The best match so far
%% Paths - A proplist of paths. Key is the list of words, value is the status, which could be either 'live' (continue looking here), 'dead' (don't continue), or Pid (actively looking here)
%% Dictionary - A list of words that may be used.

% Exit clauses
search(_Target, Match, [],  _Dictionary) -> % ran out of paths to search
  Match;
search(_Target, [], _Paths,  []) -> % ran out of dictionary to search
  failure;

% Entry / loop clause
search(Target, Match, Paths, Dictionary) ->
  io:format("Path set is ~p~n~n", [Paths]),
  % Check for early exit if all paths are dead
  NonDeadPaths = lists:filter(fun({_Key, Status}) -> Status =/= dead end, Paths),

  % Get the list of minimum length paths
  LivePaths = lists:filtermap(
    fun({Path, Status}) ->
      case Status of live ->
        {true, length(Path)};
        _ -> false
      end
    end,
  Paths), 
  MinLength = if LivePaths =:= [] -> infinity;
    LivePaths =/= [] -> lists:min(LivePaths)
  end,

  % Spawn search processes
  Whoami = self(),
  PathsSpawned = lists:map(fun({Path, Status}) ->
    case Status of live
      when length(Path) =:= MinLength ->
        io:format("Spawning ~p search~n", [Path]),
        {Path, spawn(fun() -> Whoami ! {self(), search_neighbors(Path, Target, Dictionary)},
          io:format("~p is done~n", [self()])

        end)};
      _ -> {Path, Status}
    end      
  end, Paths),
  receive
    {_From, {dead, Path}} ->
      io:format("Removing path ~p from ~p~n", [Path, PathsSpawned]),
      NewPaths = combine_path_lists([{Path, dead}], PathsSpawned),
      NewMatch = Match,
      NewDictionary = Dictionary;
    {_From, {found, MatchContender, Examined}} ->
      % io:format("Found match ~p~n",[MatchContender]),
      NewMatch = if
        length(Match) < length(MatchContender) , Match =/= [] -> Match;
        true -> MatchContender
      end,
      NewDictionary = Dictionary -- Examined,
      MatchBase = tl(MatchContender), % this is the base of the match, which we must mark as dead so as to avoid revisiting

      % we remove paths that are longer than the best match
      NewPaths = 
        lists:map(fun({Key, Status}) ->
          NewStatus = case length(Key) >= length(NewMatch) of
            true -> dead;
            false -> Status
          end,
          {Key, NewStatus}
        end, combine_path_lists([{MatchBase, dead}], PathsSpawned));
    {_From, {notfound, PathContenders}} -> 
      % io:format("Not found but let's keep looking~n", []),
      NewMatch = Match,
      NewPaths =
        combine_path_lists( 
          % old portion (taking hd of the first one because they should all have the same nthtail)
          lists:keydelete(tl(hd(PathContenders)), 1, PathsSpawned), 
          % new paths
          [{Path, live} || Path <- PathContenders]
        ),
      NewDictionary = Dictionary -- lists:map(fun(NewPath) -> hd(NewPath) end, PathContenders) % remove visited members of dictionary
    after 0 -> 
      io:format("Nothing~n"),
      NewPaths = Paths,
      NewMatch = Match,
      NewDictionary = Dictionary
  end,


  % If all the paths are dead, we pass an empty pathset to prompt function return
  NewPathsNonDead = case NonDeadPaths =:= [] of true -> [];
    false -> NewPaths
  end,
  search(Target, NewMatch, NewPathsNonDead, NewDictionary).


%% Simple helper function to combine two sets of paths. Where their keys overlap the first one is used.
%%
%% PathsFavored - A preferred proplist of paths
%% PathsSecondary - A proplist of paths
combine_path_lists(PathsFavored, PathsSecondary) ->
  CombinedPaths = PathsFavored ++ PathsSecondary,
  lists:map(fun(Key) ->  {Key, proplists:get_value(Key, CombinedPaths)} end, proplists:get_keys(CombinedPaths)).


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
      {found, [Target | CurrentPath], Neighbors};
    false when Neighbors =:= [] -> 
      {dead, CurrentPath};
    false ->
      NewPaths = [[H] ++ T || H <- Neighbors, T <- [CurrentPath]],
      {notfound, NewPaths}
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
  {notfound,[["abdde","abcde","abcdx"], ["abqde","abcde","abcdx"]]} = search_neighbors(["abcde", "abcdx"], "absde", ["abqde","abdde", "abbbb", "absdf"]),

  % combine_path_lists
  [{x,50},{y,60},{z,0}] = combine_path_lists([{x, 50}, {y, 60}], [{y, 0}, {z, 0}]),

  % search
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],[{["bbcde","abcde"], live}],["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],[{["bbcde","abcde"], live}, {["abcdf","abcde"], live}],["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbdf","bbbde","bbcde","abcde"] =
    search( "bbbdf", [],[{["bbcde","abcde"], live}, {["abcdf","abcde"], live}],["bbbde", "bbcce", "abbde", "bqddd", "bbbdf"] ),
  [] = search( "abccf", [],[{["bbcde","abcde"], live}],["abcdd", "abbcc", "abccc", "abcce", "bbcde", "bacde", "aacde"] ),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], [{["abcde"], live}], ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abbbe", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  [] = search( "abbbb", [], [{["abcde"], live}], ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], [{["abcde"], live}], ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbeb"] = search( "abbbb", [], [{["abcde"], live}, {["bbbde"], live}, {["abbeb"], live}], ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),

  ok.