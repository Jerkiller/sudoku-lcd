package loader

import (
	"fmt"
	"os"
	"strings"

	"sudoku/go-libs/types"
)

func LoadMany() (res []types.Sudoku) {
	for i := 0; i <= 61; i++ {
		fileName := fmt.Sprintf("samples/%d.txt", i)
		res = append(res, Load(fileName))
	}
	return
}

func Load(file string) (b types.Sudoku) {
	dat, err := os.ReadFile(file)
	if err != nil {
		panic(err)
	}
	rows := strings.Split(string(dat), "\n")
	b.Init()
	for i := 0; i < len(rows); i++ {
		cells := strings.Split(rows[i], " ")
		for j := 0; j < len(cells); j++ {
			b.Board[i][j] = []uint8(cells[j])[0] - 48
		}
	}
	return
}
