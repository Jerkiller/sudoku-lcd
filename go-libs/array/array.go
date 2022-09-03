package array

func OneToNine() []uint8 {
	return []uint8{1, 2, 3, 4, 5, 6, 7, 8, 9}
}

func Repeat[T any](size int, v T) []T {
	result := make([]T, 0, size)
	for i := 0; i < size; i++ {
		result = append(result, v)
	}
	return result
}

func IsIn[T comparable](a T, list []T) bool {
	for _, b := range list {
		if b == a {
			return true
		}
	}
	return false
}

func RemoveZeros(list []uint8) (result []uint8) {
	for _, item := range list {
		if item != 0 {
			result = append(result, item)
		}
	}
	return
}

// Set Difference: A - B
func Difference[T comparable](a, b []T) (diff []T) {
	m := make(map[T]bool)
	for _, item := range b {
		m[item] = true
	}
	for _, item := range a {
		if _, ok := m[item]; !ok {
			diff = append(diff, item)
		}
	}
	return
}

// Set Union: A U B
func Union[T comparable](a, b []T) (union []T) {
	for _, item := range a {
		union = append(union, item)
	}
	for _, item := range b {
		if !IsIn(item, union) {
			union = append(union, item)
		}
	}
	return
}

func HasZeros(a []uint8) bool {
	for _, item := range a {
		if item == 0 {
			return true
		}
	}
	return false
}

func IsIllegalSequence(lst []uint8) bool {
	for _, i := range OneToNine() {
		if CountOccurrences(lst, i) > 1 {
			return true
		}
	}
	return false
}

func CountOccurrences(lst []uint8, elem uint8) (occurrences int) {
	for _, x := range lst {
		if elem == x {
			occurrences++
		}
	}
	return occurrences
}

func Test() {
	// 	var a = []uint8{0, 1, 2, 3, 4, 5}
	// 	var b = []uint8{2, 3, 5, 7, 11}
	// 	fmt.Printf("Running Array tests\n")
	// 	fmt.Printf("A: ")
	// 	fmt.Println(a)
	// 	fmt.Printf("B: ")
	// 	fmt.Println(b)
	// 	fmt.Printf("A without Zeros: ")
	// 	fmt.Println(RemoveZeros(a))
	// 	fmt.Printf("A - B: ")
	// 	fmt.Println(Difference(a, b))
	// 	fmt.Printf("A U B: ")
	// 	fmt.Println(Union(a, b))
	// fmt.Println(IsIllegalSequence([]uint8{0, 0, 0, 5, 4, 1, 6, 7, 8, 9}))
}
