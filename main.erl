-module(main).
-import(reader,[read_sudoku/1]).
-import(printer,[print_sudoku/1]).
-export([
  main/1, start/0,
  flatten/1, is_list_correct/1,
  is_complete/1, is_correct/1,
  get_row/2, get_col/2, get_box/2, get_cell/3,
  cell_to_box/2, intersect/2, replace/3, apply_move/2,
  is_x_in_box/2, missing/1, print_lst/1,
  get_sudoku/0, get_sudoku/1,
  get_all_sudoku_boards/0,
  solve_all_sudoku_boards/0,
  get_sudoku_boards_by_position/2,
  get_sudoku_from_move_list/2,
  get_moves_by_cell/3,
  solve/1]).

flatten(X) -> lists:reverse(flatten(X,[])).
flatten([],Acc) -> Acc;
flatten([H|T],Acc) when is_list(H) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc) -> flatten(T,[H|Acc]).

get_row(_, Row) when Row < 1 ->
  error("Cannot access a row smaller than 1");
get_row(_, Row) when Row > 9 ->
  error("Cannot access a row bigger than 9");
get_row(Sudoku, Row) ->
  lists:nth(Row, Sudoku).

get_col(_, Col) when Col < 1 ->
  error("Cannot access a column smaller than 1");
get_col(_, Col) when Col > 9 ->
  error("Cannot access a column bigger than 9");
get_col(Sudoku, Col) ->
  lists:map(
    fun(Row) -> lists:nth(Col, Row) end,
    Sudoku
  ).

is_x_in_box(_, Box) when Box > 3 ->
  error("Box must be a value smaller than 3");
is_x_in_box(_, Box) when Box < 1 ->
  error("Box must be a value bigger than 1");
is_x_in_box(X, Box) ->
  ((X-1) div 3) =:= (Box-1).

% 1 1 -> 1
% 1 2 +> 2
% 1 3 -> 3
% 2 1 -> 4
% 2 2 -> 5
get_box(Sudoku, Box) ->
  get_box(Sudoku, ((Box+2) rem 3)+1, ((Box+2) div 3), 1, 1).
get_box([], _, _, _, _) -> [];
get_box([[]|Rest], BoxRow, BoxCol, _, Y) ->
  get_box(Rest, BoxRow, BoxCol, 1, Y+1);
get_box([[Head|Row]|Rest], BoxRow, BoxCol, X, Y) ->
  case is_x_in_box(X, BoxRow) and is_x_in_box(Y, BoxCol) of
    true -> [Head|get_box([Row|Rest], BoxRow, BoxCol, X+1, Y)];
    false -> get_box([Row|Rest], BoxRow, BoxCol, X+1, Y)
  end.

get_cell(Sudoku, X, Y) ->
  lists:nth(Y, lists:nth(X, Sudoku)).

is_complete(Sudoku) ->
  lists:all(
    fun(Cell) -> Cell =/= 0 end,
    flatten(Sudoku)
  ).

% Check if a seq of number is valid in sudoku rules e.g. 0 1 2 3 0 0 0 4 5 is valid, 5 5 5 is not valid
is_list_correct([]) ->
  true;
is_list_correct([T|L]) when T =:= 0 ->
  is_list_correct(L);
is_list_correct([T|L]) ->
  not(lists:member(T, L)) and is_list_correct(L).

are_rows_correct(Sudoku) -> lists:all(
  fun(X) -> is_list_correct(get_row(Sudoku, X)) end,
  lists:seq(1, 9)
).

are_cols_correct(Sudoku) -> lists:all(
  fun(X) -> is_list_correct(get_col(Sudoku, X)) end,
  lists:seq(1, 9)
).

are_boxes_correct(Sudoku) -> lists:all(
  fun(X) -> is_list_correct(get_box(Sudoku, X)) end,
  lists:seq(1, 9)
).

is_correct(Sudoku) -> 
  are_rows_correct(Sudoku) and are_cols_correct(Sudoku) and are_boxes_correct(Sudoku).

missing(Series) ->
  lists:seq(1, 9) --
  lists:filter(
    fun(X) -> X =/= 0 end,
    Series
  ).

print_lst([]) -> io:fwrite("\n");
print_lst([H|T]) ->
  io:fwrite("~w ", [H]),
  print_lst(T).

get_sudoku() -> read_sudoku("samples/3.txt").
get_sudoku(Id) -> read_sudoku("samples/"++ integer_to_list(Id) ++".txt").
get_all_sudoku_boards() -> lists:map(fun(I) -> get_sudoku(I) end, lists:seq(0, 30)).

cell_to_box(X, Y) -> ((Y+2) div 3)+((X-1) div 3)*3.

%% 
% @param A First list
% @param B Second list
% @returns The intersection between first and second list
intersect(_, []) -> [];
intersect([], _) -> [];
intersect([H1|T1], L2) ->
  case lists:member(H1, L2) of
    true -> lists:uniq([H1|intersect(T1, L2)]);
    false -> intersect(T1, L2)
  end.
intersect(A,B,C) -> intersect(intersect(A,B),C).

%% 
% @param Sudoku A sudoku board (list of 9 lists)
% @param X position of X (1-9)
% @param Y position of Y (1-9)
% @returns A list of possible candidates for that position
candidates(Sudoku, X, Y) ->
  RowCandidates = missing(get_row(Sudoku,X)),
  ColCandidates = missing(get_col(Sudoku,Y)),
  BoxCandidates = missing(get_box(Sudoku,cell_to_box(X,Y))),
  % This is needed in order to perform a move after a guess
  % otherwise, if we try to compute the best move every time,
  % we end up with an empty list of candidates
  % and we never apply a wrong move. Instead we apply the fail-fast philosophy
  if
    length(RowCandidates) =:= 1 -> RowCandidates;
    length(ColCandidates) =:= 1 -> ColCandidates;
    length(BoxCandidates) =:= 1 -> BoxCandidates;
    true -> intersect(RowCandidates, ColCandidates, BoxCandidates)
  end.

%% replace(2, 44, [1,2,3]) --> [1,44,3]
replace(ElemKey, NewElem, List) ->
  lists:sublist(List,ElemKey-1) ++ [NewElem] ++ lists:nthtail(ElemKey,List).  

%% Apply a move: put NewCell in position (X,Y)
apply_move(Sudoku, {X, Y, NewCell}) ->
  lists:sublist(Sudoku,X-1) ++ [
    replace(Y, NewCell, lists:nth(X,Sudoku))
    ] ++ lists:nthtail(X,Sudoku).  

apply_obvious_move(Sudoku) ->
  apply_obvious_move(Sudoku,1,1).
apply_obvious_move(Sudoku,X,Y) ->
  case (get_cell(Sudoku,X,Y) =:= 0) and (length(candidates(Sudoku,X,Y)) =:= 1) of
    true -> NewCell = lists:nth(1,candidates(Sudoku,X,Y)),
            % io:fwrite("-> Apply move ~w in (~w, ~w)~n",[NewCell, X, Y]),
            apply_move(Sudoku, {X, Y, NewCell});
    false ->
      case Y =:= 9 of
        false -> apply_obvious_move(Sudoku,X,Y+1);
        true ->
          case X =:= 9 of
            false -> apply_obvious_move(Sudoku,X+1,1);
            true -> Sudoku
          end
      end
  end.

apply_all_obvious_moves(Sudoku) ->
  Applied = apply_obvious_move(Sudoku),
  case Applied =:= Sudoku of
    true -> Applied; % io:fwrite("No more~n"),Applied;
    false -> apply_all_obvious_moves(Applied)
  end.

get_best_guess(Sudoku) -> get_best_guess(Sudoku, 0, 0, 9, 1, 1).
get_best_guess(Sudoku, BestX, BestY, BestCandidates, X, Y) ->
  %  io:fwrite("(~w,~w) --> ~w~n",[X,Y,length(candidates(Sudoku,X,Y))]),
   case (get_cell(Sudoku,X,Y) =:= 0) and (length(candidates(Sudoku,X,Y)) < BestCandidates) of
    true -> get_best_guess(Sudoku, X, Y, length(candidates(Sudoku,X,Y)), X, Y);
    false ->
      case Y =:= 9 of
        false -> get_best_guess(Sudoku, BestX, BestY, BestCandidates,X,Y+1);
        true ->
          case X =:= 9 of
            false -> get_best_guess(Sudoku, BestX, BestY, BestCandidates,X+1,1);
            true -> {BestX, BestY}
          end
      end
  end.

get_sudoku_boards_by_position(Sudoku, {X, Y}) ->
  get_sudoku_from_move_list(Sudoku, get_moves_by_cell(Sudoku, X, Y)).

% From a list of moves, get a list of Sudoku boards with those moves applied
get_sudoku_from_move_list(Sudoku, Moves) ->
  lists:map(fun(Move) -> apply_move(Sudoku, Move) end, Moves).

% Given a sudoku and a position, get the list of moves that can be applied there
get_moves_by_cell(Sudoku, X, Y) ->
  lists:map(fun(Value) -> {X, Y, Value} end, candidates(Sudoku, X, Y)).

check_sudoku(Sudoku) ->
  case not(is_correct(Sudoku)) of
    true -> exit(wrongSudoku); % io:fwrite("Sudoku WRONG~n"), exit(wrongSudoku);
    false -> ok
  end,
  case is_complete(Sudoku) of
    true -> print_sudoku(Sudoku), exit(finishedSudoku); % io:fwrite("Sudoku completed!!!~n"), print_sudoku(Sudoku), exit(finishedSudoku);
    false -> ok
  end.

solve(Sudoku) ->
  check_sudoku(Sudoku),
  NewSudoku = apply_all_obvious_moves(Sudoku),
  check_sudoku(NewSudoku),
  SudokuBoards = get_sudoku_boards_by_position(Sudoku, get_best_guess(Sudoku)),
  lists:map(fun(S) -> spawn(main, solve, [S]) end, SudokuBoards).
  % Pids = lists:map(fun(S) -> spawn(main, solve, [S]) end, SudokuBoards),
  % io:fwrite("PIDS \n"),
  % print_lst(Pids).

solve_all_sudoku_boards() -> 
  Boards = get_all_sudoku_boards(),
  lists:map(fun(S) -> spawn(main, solve, [S]) end, Boards).
  % lists:map(fun(B) -> solve(B) end, Boards).

start() -> solve_all_sudoku_boards().

main(FileId) ->
  % io:fwrite("Reading Sudoku~nsudoku-samples/"++FileId++".txt~n"),
  Sudoku = read_sudoku("sudoku-samples/"++FileId++".txt"),
  % print_sudoku(Sudoku),
  % io:fwrite("~nSolving...~n"),
  solve(Sudoku).
