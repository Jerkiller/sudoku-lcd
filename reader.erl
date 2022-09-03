-module(reader). 
-export([read_lines/1, read_sudoku/1]). 

open_file(FileName, Mode) ->
  {ok, Device} = file:open(FileName, [Mode]),
  Device.

close_file(Device) ->
  ok = file:close(Device).

read_lines(File) ->
  case file:read_line(File) of
    {ok, Data} -> [Data | read_lines(File)];
    eof        -> []
  end.

split_lines([]) ->
  [];
split_lines([T|L]) ->
  Tokens = string:tokens(string:trim(T), " "),
  Digits = lists:map(fun(X) -> list_to_integer(X) end, Tokens),
  [Digits|split_lines(L)].

read_sudoku(FileName) ->
  File = open_file(FileName, read),
  Data = read_lines(File),
  close_file(File),
  split_lines(Data).