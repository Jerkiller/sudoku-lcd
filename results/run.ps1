$X=1; Invoke-Expression ("go run main.go -sudoku="+$($X)+" -workers=100 -buffer=100000; echo ''; erl -noshell -run main main "+$($X)+" -s init stop")


for (($X = 0); $X -lt 10; $X++)
{
    Invoke-Expression ("go run main.go -sudoku="+$($X)+" -workers=100 -buffer=100000")
}

for (($X = 0); $X -lt 10; $X++)
{
    Invoke-Expression ("erl -noshell -run main main "+$($X)+" -s init stop")
}
