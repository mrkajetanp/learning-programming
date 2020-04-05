package main

import (
	"fmt"
	"math"
	"strings"
)

func pointers() {
	i, j := 42, 2701

	p := &i
	fmt.Println(*p)

	*p = 21
	fmt.Println(i)

	p = &j
	*p = *p / 37
	fmt.Println(j)
}

// Vertex simply represents a vertex
type Vertex struct {
	X int
	Y int
}

func structs() {
	fmt.Println(Vertex{1, 2})
	v := Vertex{0, 3}
	v.X = 4
	fmt.Println(v.X, v.Y, v)

	p := &v
	(*p).X = 1e9
	p.Y = 1e9
	fmt.Println(v)

	p2 := &Vertex{1, 3}
	fmt.Println(p2)
	fmt.Println(Vertex{X: 2})
	fmt.Println(Vertex{})
}

func arrays() {
	var a [2]string
	a[0] = "Hello"
	a[1] = "World"
	fmt.Println(a[0], a[1])
	fmt.Println(a)

	primes := [6]int{2, 3, 5, 7, 11, 13}
	fmt.Println(primes)

	var s []int = primes[1:5]
	fmt.Println(s)

	names := [4]string{
		"John",
		"Paul",
		"George",
		"Ringo",
	}
	fmt.Println(names)

	a1 := names[0:2]
	b1 := names[1:3]
	fmt.Println(a1, b1)

	b1[0] = "XXX"
	fmt.Println(a1, b1)
	fmt.Println(names)

	q := []int{2, 3, 5, 7, 11, 13}
	fmt.Println(q)

	// q = q[2:]
	// fmt.Println(q)

	fmt.Println("Length: ", len(q))
	fmt.Println("Length: ", cap(q))

	// Nil slices

	var s1 []int
	fmt.Println(s1, len(s1), cap(s1))
	if s1 == nil {
		fmt.Println("nil!")
	}
}

func printSlice(s string, x []int) {
	fmt.Printf("%s len=%d cap=%d %v\n",
		s, len(x), cap(x), x)
}

func printSlice2(s []int) {
	fmt.Printf("len=%d cap=%d %v\n", len(s), cap(s), s)
}

func moreOnSlices() {
	a := make([]int, 5)
	printSlice("a", a)

	b := make([]int, 0, 5)
	printSlice("b", b)

	// Create a tic-tac-toe board.
	board := [][]string{
		[]string{"_", "_", "_"},
		[]string{"_", "_", "_"},
		[]string{"_", "_", "_"},
	}

	// The players take turns.
	board[0][0] = "X"
	board[2][2] = "O"
	board[1][2] = "X"
	board[1][0] = "O"
	board[0][2] = "X"

	for i := 0; i < len(board); i++ {
		fmt.Printf("%s\n", strings.Join(board[i], " "))
	}

	var s []int
	printSlice2(s)

	// append works on nil slices.
	s = append(s, 0)
	printSlice2(s)

	// The slice grows as needed.
	s = append(s, 1)
	printSlice2(s)

	// We can add more than one element at a time.
	s = append(s, 2, 3, 4)
	printSlice2(s)
}

func ranges() {
	var pow = []int{1, 2, 4, 8, 16, 32, 64, 128}
	for i, v := range pow {
		fmt.Printf("2**%d = %d\n", i, v)
	}

	// for _, v := range pow {
	for i := range pow {
		fmt.Println(i)
	}
}

func maps() {
	var m map[string]int
	m = make(map[string]int)

	m["one"] = 1
	m["two"] = 2
	m["three"] = 3
	fmt.Println(m)
	fmt.Println("one", m["one"])

	var m2 = map[string]int{
		"google": 22,
	}
	fmt.Println(m2)

	elem, ok := m["three"]
	fmt.Println(elem, ok)
	elem, ok = m["four"]
	fmt.Println(elem, ok)

	delete(m, "two")
	fmt.Println(m)
}

func compute(fn func(float64, float64) float64) float64 {
	return fn(3, 4)
}

func functionValues() {
	hypot := func(x, y float64) float64 {
		return math.Sqrt(x*x + y*y)
	}

	fmt.Println(hypot(5, 12))
	fmt.Println(compute(hypot))
	fmt.Println(compute(math.Pow))
}

// Closures
func adder() func(int) int {
	sum := 3

	return func(x int) int {
		sum += x
		return sum
	}
}

func closures() {
	ad := adder()
	fmt.Println(ad(5))
	fmt.Println(ad(7))
	fmt.Println(ad(5))
}

// MoreTypes executes functions in this file
func MoreTypes() {
	fmt.Println("*** Flow Control ***")
	fmt.Println()

	pointers()
	structs()
	arrays()
	moreOnSlices()
	ranges()
	maps()
	functionValues()
	closures()
}
