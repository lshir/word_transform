-module(word_transform).
-compile(export_all).


% Will start the judge, which keeps track of matches and the success of the search
%
search(StartWord, TargetWord, Dictionary) ->
  judge(StartWord, TargetWord, [], [[{[StartWord], live}]], Dictionary).
  % {Status, Neighbors} = search_neighbors([StartWord], TargetWord, Dictionary),
  % case Status of 
  %   found -> {found, [StartWord, TargetWord]};
  %   dead -> {no_match};
  %   notfound -> 
  %     NeighborPaths = [{Path, live} || Path <- Neighbors],
  %     judge(StartWord, TargetWord, [], NeighborPaths, Dictionary -- lists:append(Neighbors))
  % end.
  




% Paths will be structured as : 
% [word sequence], live / Pid


% Exit clauses
judge(_Start, _Target, Match, [],  _Dictionary) -> % ran out of paths to search
  Match;
judge(_Start, _Target, [], _Paths,  []) -> % ran out of dictionary to search
  failure;
% Entry / loop clause
judge(Start, Target, Match, Paths, Dictionary) ->
  % Get the list of minimum length paths
  MinLength = lists:min(lists:filtermap(
    fun({Path, Status}) ->
      case Status of live ->
        {true, length(Path)};
        _ -> false
      end
    end,
  Paths)),

  % Spawn search processes
  Whoami = self(),
  _MinPaths = lists:map(fun({Path, Status}) ->
    case Status of live
      when length(Path) =:= MinLength ->
        {Path, spawn(fun() -> Whoami ! {self(), search_neighbors(Path, Target, Dictionary)} end)};
      _ -> {Path, Status}
    end      
  end, Paths),
  receive
    {_From, {dead, Path}} ->
      io:format("Removing path ~p~n", [Path]),
      NewPaths = lists:keydelete(Path, 1, Paths),
      NewMatch = Match,
      NewDictionary = Dictionary;
    {_From, {found, MatchPretender, Examined}} ->
      io:format("Found match ~p~n",[MatchPretender]),
      NewMatch = if
        length(Match) < length(MatchPretender) , Match =/= [] -> Match;
        true -> MatchPretender
      end,
      NewDictionary = Dictionary -- Examined,
      NewPaths = lists:keydelete(lists:nthtail(1, MatchPretender), 1, Paths);
    {_From, {notfound, PathContenders}} -> 
      io:format("Not found but let's keep looking~n", []),
      NewMatch = Match,
      NewPaths =
        lists:append(
          lists:keydelete(lists:nthtail(1, lists:nth(1, PathContenders)), 1, Paths), % old portion
          [{Path, live} || Path <- PathContenders] % new paths
        ),
      NewDictionary = Dictionary -- lists:map(fun(NewPath) -> lists:nth(1, NewPath) end, PathContenders) % remove visited members of dictionary
    after 0 -> 
      io:format("Nothing~n"),
      NewPaths = Paths,
      NewMatch = Match,
      NewDictionary = Dictionary
  end,

  judge(Start, Target, NewMatch, NewPaths, NewDictionary)

  % judge(Start, Target, NewMatch, NewPaths, NewDictionary)
  .



% Reads in the file as a path and returns the words in a list.
% @todo for the success of our search, we need to check that the target is in the dictionary
read_dictionary(File) ->
  {ok, Binary} = file:read_file(File),
  string:tokens(binary_to_list(Binary), "\n").

% The assumption here is that our alphabet is lowercase a-z, but we want this to be flexible enough to handle other alphabets.
define_alphabet() ->
  lists:map(fun(X) -> [X] end, lists:seq(97, 122)).


% Generates candidates set from a current word and a dictionary
find_neighbors(CurrentWord, Dictionary) -> 
  Candidates = generate_candidates(CurrentWord),
  lists:filter(fun(Word) -> lists:member(Word, Dictionary) end, Candidates).


% Wraps find_neighbors to test if we have a match, dead, or a new set of candidates
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




% Generates a set of all words that could possibly be reached from the current one (without regard for the dictionary)
%
% Loops over each letter in the word, does a Cartesian expansion with all possible substitutions
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




% Could be made nicer -
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
   {notfound,[["abdde","abcde","abcdx"], ["abqde","abcde","abcdx"]]} = search_neighbors(["abcde", "abcdx"], "absde", ["abqde","abdde", "abbbb", "absdf"]),

   % judge
   judge("abcde", "abccf", [],[{["bbcde","abcde"], live},{["aacde","abcde"], live},{["abcce","abcde"], live},{["abcdd","abcde"], live}],["abcdd", "abbcc", "abccc", "abcce", "bbcde", "bacde", "aacde"] ).


   % ok.