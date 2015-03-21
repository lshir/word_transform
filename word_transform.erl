-module(word_transform).
-compile(export_all).

% Reads in the file as a path and returns the words in a list.
read_dictionary(File) ->
  {ok, Binary} = file:read_file(File),
  string:tokens(binary_to_list(Binary), "\n").

% The assumption here is that our alphabet is lowercase a-z, but we want this to be flexible enough to handle other alphabets.
define_alphabet() ->
  lists:map(fun(X) -> [X] end, lists:seq(97, 122)).


% % Generates candidates set from a current word and a dictionary
% % @todo add tracking word path
% % @todo add argument for target
% % @todo format return to parent process
% find_neighbors(CurrentWord, Dictionary) -> 
%   % lists:splitwith?
%   


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