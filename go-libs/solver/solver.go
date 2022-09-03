package solver

import (
	"fmt"
	"sudoku/go-libs/types"
	"time"
)

type Solver struct {
	Res chan types.Sudoku
	Win chan types.Sudoku
}

func ApplyObviousMoves(sudoku types.Sudoku) types.Sudoku {
	if !sudoku.IsComplete() {
		for rowIndex, row := range sudoku.Board {
			for colIndex, item := range row {
				if item == 0 {
					candidates := sudoku.FindCandidates(rowIndex, colIndex)
					if len(candidates) == 1 {
						newSudoku := sudoku.Apply(types.Move{
							I:     rowIndex,
							J:     colIndex,
							Value: candidates[0],
						})
						return ApplyObviousMoves(newSudoku)
					}
				}
			}
		}
	}
	return sudoku
}

func ApplyPossibleMoves(sudoku types.Sudoku) types.Sudoku {
	newSudoku := ApplyObviousMoves(sudoku)
	// Apply other reasoning
	return newSudoku
}

// func Solve(sudoku types.Sudoku) string {
// 	newSudoku := ApplyPossibleMoves(sudoku)
// 	if newSudoku.IsComplete() {
// 		return "WIN"
// 	}
// 	if !newSudoku.IsCorrect() {
// 		return "LOSE"
// 	}
// 	i, j := sudoku.FindFirstEmptyCell()
// 	candidates := sudoku.FindCandidates(i, j)
// 	if len(candidates) == 0 {
// 		return "LOSE"
// 	}
// 	// fmt.Println("Trying to guess... Candidates: ", candidates)
// 	guessed := newSudoku.Apply(types.Move{
// 		I:     i,
// 		J:     j,
// 		Value: candidates[0],
// 	})
// 	return Solve(guessed)
// }

func (s *Solver) Run() string {
	sudoku := <-s.Res
	newSudoku := ApplyPossibleMoves(sudoku)
	if !newSudoku.IsCorrect() {
		fmt.Println("UNCORRECT")
		return s.Run()
	} else if newSudoku.IsComplete() {
		fmt.Println("WIN")
		s.Win <- newSudoku
		return "WIN"
	} else {
		// fmt.Println("GUESS")
		i, j := newSudoku.FindFirstEmptyCell()
		candidates := newSudoku.FindCandidates(i, j)
		if len(candidates) == 0 {
			// fmt.Println("LOSE")
			return s.Run()
		}
		// fmt.Println("candidates", candidates)
		for _, candidate := range candidates {
			// fmt.Println("guess ",index,candidate)
			candidateRes := newSudoku.Apply(types.Move{
				I:     i,
				J:     j,
				Value: candidate,
			})

			select {
			case s.Res <- candidateRes:
			// Buffer is full, try to wait a bit
			case <-time.After(3 * time.Second):
				fmt.Println("timeout 3")
			}
		}
	}

	return s.Run()
}
