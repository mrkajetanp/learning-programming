package main

import (
	"fmt"
	"math"
	"runtime"
	"time"
)

func forLoop() {
	sum := 0
	for i := 0; i < 10; i++ {
		sum += i
	}
	fmt.Println("Sum:", sum)

	sum = 1
	for sum < 100 {
		sum += sum
	}
	fmt.Println("Sum:", sum)
}

func whileLoop() {
	sum := 1
	for sum < 1000 {
		sum += sum
	}
	fmt.Println("Sum:", sum)

	for {
		break
	}
	fmt.Println("Broken!")
}

func sqrt(x float64) string {
	if x < 0 {
		return (sqrt(-x)) + "i"
	}
	return fmt.Sprint(math.Sqrt(x))
}

func pow(x, n, lim float64) float64 {
	if v := math.Pow(x, n); v < lim {
		return v
	} else {
		fmt.Println(v * 2)
	}

	return lim
}

func switches() {
	switch os := runtime.GOOS; os {
	case "darwin":
		fmt.Println("OS X")
	case "linux":
		fmt.Println("Linux")
	default:
		fmt.Printf("%s\n", os)
	}

	t := time.Now()
	fmt.Println(t)
	switch {
	case t.Hour() < 12:
		fmt.Println("Good morning!")
	case t.Hour() < 17:
		fmt.Println("Good afternoon.")
	default:
		fmt.Println("Good evening.")
	}
}

func ifs() {
	fmt.Println(sqrt(4), sqrt(-4))
	fmt.Println(pow(3, 2, 10))
	fmt.Println(pow(3, 3, 20))
}

func deferring() {
	// Stacking defers
	defer fmt.Println("world")
	defer fmt.Println("my")

	fmt.Println("hello")
}

// FlowControl executes all the functions in this file
func FlowControl() {
	fmt.Println("*** Flow Control ***")
	fmt.Println()

	forLoop()
	whileLoop()
	ifs()
	switches()
	deferring()

	fmt.Println()
}
