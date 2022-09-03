# Sudoku Solver

This repo contains both software pieces written in Go and in Erlang.

## Tests

In the folder "sudoku-samples" there can be found a list of sudoku boards to be solved.

## Execution

Given that we want to solve sudoku "sudoku-samples/11.txt", ww need to run the following commands:

### Go

```bash
go build
go run main.go -sudoku=11 -workers=100 -buffer=100000
```


### Erlang

```bash
erlc main.erl 
% $ erl -noshell -run main main 11 -s init stop
```
Another way is to run the command `erl` and then, into the Erlang REPL, run `c(printer). c(reader). c(main). main:main("11").`