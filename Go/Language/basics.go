package main

import (
	"fmt"
	"math"
	"math/cmplx"
	"math/rand"
)

func add(x int, y int) int {
	return x + y
}

func sub(x, y int) int {
	return x - y
}

func swap(x, y string) (string, string) {
	return y, x
}

// Naked return example
func split(sum int) (x, y int) {
	x = sum * 4 / 9
	y = sum - x
	return
}

var c, python, java = true, false, "no!"

// Basics is the main function running all the content
func Basics() {
	fmt.Println("My favourite number is:", rand.Intn(20))
	fmt.Printf("You now have %g issues\n", math.Sqrt(7))
	fmt.Println(math.Pi)
	fmt.Println("8 + 3 =", add(8, 3))
	fmt.Println("8 - 3 =", sub(8, 3))
	a, b := swap("hello", "world")
	fmt.Println("swap(hello, world) ", a, b)
	fmt.Println(split(17))

	var i, j int = 1, 2
	k := 3
	fmt.Println(i, j, k, c, python, java)

	var (
		ToBe   bool       = false
		MaxInt uint64     = 1<<64 - 1
		z      complex128 = cmplx.Sqrt(-5 + 12i)
	)

	c1 := -5 + 12i
	fmt.Println("Complex:", c1)

	fmt.Printf("Type: %T Value: %v\n", ToBe, ToBe)
	fmt.Printf("Type: %T Value: %v\n", MaxInt, MaxInt)
	fmt.Printf("Type: %T Value: %v\n", z, z)

	var newI int = 12
	var x uint = uint(newI)
	fmt.Println("x: ", x)

	const word = "helloo"
	fmt.Println()
}
