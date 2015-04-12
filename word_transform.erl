-module(word_transform).
-author('Shir Levkowitz <shir@lhaim.com>').
-export([transform/3, read_dictionary/1, tests/0]).

%% A simple concurrent application to transform one word into another of the same length through single-letter substitutions within a dictionary.

%% DESCRIPTION 
%% 
%% This performs a breadth-first tree search on paths transforming one word into another through a dictionary.
%% We can't filter the branches (paths) by some distance/similarity metric because we don't have a guarantee of convexity
%% (i.e. two words that "look" similar under most metrics could be far apart).
%%
%%  Speed could be improved by more care in the path reduction (filtering out duplicate paths to the same word), probably through keying on the head.
%%  Input validation is minimal; in particular be careful of poorly formatted dictionary (must have one word per line and nothing else), and inputs of wrong length.
%%
%% Words for dictionary sampled from http://www.morewords.com
%%

%% Examples: 
%%  > DictionaryMinimal = word_transform:read_dictionary("dictionary_minimal.txt").
%%  > word_transform:transform("barye", "bayou", DictionaryMinimal).
%%  Searching paths of length 1
%%  Searching paths of length 2
%%  No result was found!
%%  []
%%  > word_transform:transform("burry", "rings", DictionaryMinimal). 
%%  Searching paths of length 1
%%  Searching paths of length 2
%%  [ cropped ]
%%  Searching paths of length 13
%%  burry
%%  berry
%%  beery
%%  beers
%%  beets
%%  bents
%%  bints
%%  bitts
%%  bites
%%  sites
%%  situs
%%  sinus
%%  sings
%%  rings
%%  ["burry","berry","beery","beers","beets","bents","bints",
%%   "bitts","bites","sites","situs","sinus","sings","rings"]
%% 
%% For more (internal as well as external) examples see the test/0 function


%% TODO
%%
%% The following improvements could be made:
%% 1. Change dictionary to an ETS table, which would make adding/removing words constant time
%% 2. Change reduce_pathset to use a hash table, which would remove duplicates in linear time
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
  DictionaryComplete = case lists:member(TargetWord, Dictionary) of true -> Dictionary; false -> [TargetWord | Dictionary] end -- [StartWord],
  Result = search(TargetWord, [], [[StartWord]], DictionaryComplete),

  Message = case Result of [] -> "No result was found!~n";
    _ -> string:join(lists:reverse(Result), "\n")
  end,
  io:format(Message ++ "\n"),
  lists:reverse(Result).


%% Reads in the file as a path and returns the words in a list.
%%
%% File - A filename to read in
read_dictionary(File) ->
  {ok, Binary} = file:read_file(File),
  string:tokens(binary_to_list(Binary), "\n").


%%
%% INTERNAL 
%% 

%% Receives and formats the output of the threads searching for neighbors
%% 
%% Ref
collect(Ref) ->
  receive
    {_Pid, Ref, {found, Match}} -> {match, Match};
    {_Pid, Ref, {dead, _Path}} -> {live, []};
    {_Pid, Ref, {notfound, Neighbors}} -> {live, Neighbors}
  end.


%% Just to clear the message queue after we find a match
flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.


%% Executes the search
%%
%% Target - The word we are looking for 
%% Match - A match, or an empty set.
%% Paths - A list of paths, each being itself a list of words with the most recent at the head and the starter at the tail
%% Dictionary - A list of words that may be used.
  % Exit clauses
search(_Target, Match, _Paths,  _Dictionary) when Match =/= [] -> % found match
  flush(),
  Match;
search(_Target, _Match, _Paths,  []) -> % ran out of dictionary to search
  [];
search(_Target, _Match, [],  _Dictionary) -> % ran out of paths to search
  [];
  % Entry / loop clause
search(Target, _, Paths, Dictionary) ->
  % Spawn search processes
  Whoami = self(),
  Ref = make_ref(),
  io:format("Searching paths of length ~p~n", [length(hd(Paths))]),

  PathsFiltered = reduce_pathset(Paths),
  PathsSpawned = lists:map(fun(Path) ->
    {spawn(fun() -> Whoami ! {self(), Ref, search_neighbors(Path, Target, Dictionary)} end), Path}
  end, PathsFiltered),

  % Collect search results
  [{match, Match}, {live, LivePaths}, {neighbors, Neighbors}] =
    lists:foldl(fun(_PathInfo, AccIn) ->
      % We don't need all matches, so only bother waiting if we need to wait!
      case proplists:get_value(match, AccIn) =/= [] of true ->
          AccIn;
        false ->
          {Status, PathSet} = collect(Ref),
          NewPathSet = lists:append(proplists:get_value(Status, AccIn), PathSet),
          NewNeighbors = lists:append(proplists:get_value(neighbors, AccIn), [hd(NewPath) || NewPath <- NewPathSet]),
          lists:keyreplace(neighbors, 1, lists:keyreplace(Status, 1, AccIn, {Status, NewPathSet}), {neighbors, NewNeighbors})
        end
    end, [{match, []}, {live, []}, {neighbors, []}], PathsSpawned),

  NewNeighbors = Dictionary -- Neighbors,
  search(Target, Match, LivePaths, NewNeighbors).


%% Filters down the set of paths to a set with unique heads - at this point we are comparing paths against each other so we cannot 
%% Paths - A list of paths
reduce_pathset(Paths) -> 
  lists:usort(fun(PathA, PathB) -> hd(PathA) =< hd(PathB) end, Paths).

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
      {found, [Target | CurrentPath]};
    false when Neighbors =:= [] -> 
      {dead, CurrentPath};
    false ->
      NewPaths = [[H] ++ T|| H <- Neighbors, T <- [CurrentPath]],
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
  {found,["absde","abcde","abcdx"]} = search_neighbors(["abcde", "abcdx"], "absde", ["absde", "abbbb", "absdf"]),
  {dead,["abcde","abcdx"]} = search_neighbors(["abcde", "abcdx"], "absde", ["abbbb", "absdf"]),
  {notfound,
          [["abdde","abcde","abcdx"],["abqde","abcde","abcdx"]]}= search_neighbors(["abcde", "abcdx"], "absde", ["abqde","abdde", "abbbb", "absdf"]),

  % reduce_pathset
  [["a", "b", "c"]] = reduce_pathset([["a", "b", "c"], ["a", "d", "e"]]),

  % search
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],[["bbcde","abcde"]],["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbde","bbcde","abcde"] = search( "bbbde", [],[["bbcde","abcde"],  ["abcdf","abcde"]] ,["bbbde", "bbcce", "abbde", "bqddd"] ),
  ["bbbdf","bbbde","bbcde","abcde"] =
    search( "bbbdf", [],[["bbcde","abcde"], ["abcdf","abcde"]],["bbbde", "bbcce", "abbde", "bqddd", "bbbdf"] ),
  [] = search( "abccf", [],[["bbcde","abcde"]],["abcdd", "abbcc", "abccc", "abcce", "bbcde", "bacde", "aacde"] ),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], [["abcde"]], ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abbbe", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  [] = search( "abbbb", [], [["abcde"]], ["abcdd", "abcce", "abccd", "abccb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbbe","abcbe","abcde"] =
    search( "abbbb", [], [["abcde"]], ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),
  ["abbbb","abbeb"] = search( "abbbb", [],
    [["abcde"], ["bbbde"], ["abbeb"]], ["abbbe", "abcdd", "abcce", "abcce", "abdcb","abdce", "abecb" , "abebb", "abbbb", "abcbe", "abede", "abbee", "abbeb", "abbbb"]),

  % transform
  DictionaryMinimal = read_dictionary("dictionary_minimal.txt"),
  [] = transform("barye", "bayou", DictionaryMinimal),
  14 = length(transform("burry", "rings", DictionaryMinimal)),
  % slower
  Dictionary = read_dictionary("dictionary.txt"),
  10 = length(transform("smart", "brain", Dictionary)),
  9 = length(transform("smart", "boise", Dictionary)),

  ok.