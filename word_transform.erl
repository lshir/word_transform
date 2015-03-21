-module(word_transform).
-compile(export_all).

% Reads in the file as a path and returns the words in a list.
read_dictionary(File) ->
  {ok, Binary} = file:read_file(File),
  string:tokens(binary_to_list(Binary), "\n").