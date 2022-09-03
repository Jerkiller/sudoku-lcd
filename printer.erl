-module(printer).
-export([print_sudoku/1]). 

print_sudoku(S) -> io:fwrite("\n"++get_sudoku(S)).

get_sudoku([]) -> "+-------+-------+-------+\n";
get_sudoku([T|L]) when length(L) rem 3 =:= 2 ->
  get_sudoku([]) ++ get_sudoku_line(T) ++ get_sudoku(L);
get_sudoku([T|L]) -> get_sudoku_line(T) ++ get_sudoku(L).
  
get_sudoku_line([]) -> "|\n";
get_sudoku_line([T|L]) when length(L) rem 3 =:= 2 ->
  "| " ++ get_sudoku_digit(T) ++ get_sudoku_line(L);
get_sudoku_line([T|L]) -> get_sudoku_digit(T) ++ get_sudoku_line(L).

get_sudoku_digit(Digit) -> integer_to_list(Digit) ++ " ".
