package types

import (
	"fmt"
	"strings"
	"sudoku/go-libs/array"
)

type Sudoku struct {
	Board [][]uint8
}

type Move struct {
	I, J int
	Value uint8
}

// set everything to 0 0 0 ...
func (self *Sudoku) Init() {
	for i := 0; i < 9; i++ {
		self.Board = append(self.Board, array.Repeat[uint8](9, 0))
	}
}

func (self Sudoku) IsComplete() bool {
	for _, row := range self.Board {
		for _, item := range row {
			if item == 0 {
				return false
			}
		}
	}
	return true
}

func (self Sudoku) IsCorrect() bool {
	three := []int{0,1,2}
	for i := range three {
		for j := range three {
			box := self.GetBox(i,j)
			if array.IsIllegalSequence(box) {
				return false
			}
		}
	}
	for i := range array.OneToNine() {
		col := self.GetColumn(i)
		row := self.GetRow(i)
		if array.IsIllegalSequence(col) || array.IsIllegalSequence(row) {
			return false
		}
	}
	return true
}

func (self Sudoku) Copy() (res Sudoku) {
	res.Init()
	for i, row := range self.Board {
		for j, item := range row {
			res.Board[i][j] = item
		}
	}
	return
}


func (self Sudoku) Print() {
	fmt.Println("|"+strings.Repeat("-", 23)+"|" )
	for i, row := range self.Board {
		for j, item := range row {
			if(j % 3 == 0) { fmt.Printf("| " ) }
			fmt.Printf("%d ", item )
		}
		if(i % 3 == 2) { fmt.Printf("|\n|"+strings.Repeat("-", 23) ) }
		fmt.Println("|")
	}
}

func (self Sudoku) GetColumn(index int) (column []uint8) {
	for _, row := range self.Board {
		column = append(column, row[index])
	}
	return
}

func (self Sudoku) GetRow(index int) []uint8 {
	return self.Board[index]
}

// 0 < (i, j) < 2
// Get all the elements in a box
func (self Sudoku) GetBox(i, j int) (box []uint8) {
	for rowIndex, row := range self.Board {
		if rowIndex / 3 == i {
			for colIndex, item := range row {
				if colIndex / 3 == j {
					box = append(box, item)
				}
			}
		}
	}
	return
}

func (self Sudoku) FindCandidates(i, j int) []uint8 { 

	rowsCandidates := array.Difference(
		array.OneToNine(),
		array.RemoveZeros(self.GetRow(i)),
	)

	colsCandidates := array.Difference(
		array.OneToNine(),
		array.RemoveZeros(self.GetColumn(j)),
	)

	boxCandidates := array.Difference(
		array.OneToNine(),
		array.RemoveZeros(self.GetBox(i/3, j/3)),
	)

	if len(rowsCandidates) == 1 { return rowsCandidates }
	if len(colsCandidates) == 1 { return colsCandidates }
	if len(boxCandidates) == 1 { return boxCandidates }

	return array.Difference(
		array.OneToNine(),
		array.RemoveZeros(
			array.Union(
				array.Union(
					self.GetRow(i),
					self.GetColumn(j),
				),
				self.GetBox(i/3, j/3),
			),
		),
	)
}

func (self Sudoku) Apply(m Move) Sudoku {
	newSudoku := self.Copy()
	newSudoku.Board[m.I][m.J] = m.Value
	return newSudoku
}

func (self Sudoku) FindFirstEmptyCell() (int, int) {
	for rowIndex, row := range self.Board {
		for colIndex, item := range row {
			if item == 0 {
				return rowIndex, colIndex
			}
		}
	}
	return -1, -1
}
