package main

import (
	"flag"
	"fmt"
	"strconv"

	"sudoku/go-libs/loader"
	"sudoku/go-libs/solver"
	"sudoku/go-libs/types"
)

// go run main.go -sudoku=1 -workers=1000 -buffer=1000
func main() {
	{
		sudokuIndex := flag.Int("sudoku", 42, "a numerical Sudoku Index (0-63)")
		workers := flag.Int("workers", 100, "the number of solver")
		maxBuffer := flag.Int("buffer", 10000000, "the length of buffered channel for unsolved sudokus")
		flag.Parse()
		// fmt.Println("\nSudoku")
		c := loader.Load("sudoku-samples/" + strconv.Itoa(*sudokuIndex) + ".txt")
		// sudokus := loader.LoadMany()
		sudokus := []types.Sudoku{c}
		channel := make(chan types.Sudoku, *maxBuffer)
		winChannel := make(chan types.Sudoku)

		for w := 0; w < *workers; w++ {
			solverWorker := solver.Solver{
				Res: channel,
				Win: winChannel,
			}
			go solverWorker.Run()
		}

		for _, sudoku := range sudokus {
			// fmt.Println("Solving Sudoku ", index)
			channel <- sudoku
			solution := <-winChannel
			solution.Print()
			fmt.Print("\n")
		}
	}
}
