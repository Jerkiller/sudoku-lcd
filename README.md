# Sudoku Solver

This repo contains two Sudoku Solvers written in [Go](https://go.dev/) and in [Erlang](https://www.erlang.org/) languages, written for my exam of *Languages of Concurrency and Distribution*.

The main goal is to write software that leverages on concurrency (and as a consequence, parallelism), using Go-Routines and Erlang Processes and message-passing communication.

## Tests

In the folder "sudoku-samples" there can be found a list of sudoku boards to be solved.

## Execution

Given that we want to solve sudoku "sudoku-samples/11.txt", we need to run the following commands:

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