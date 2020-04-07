package main

import (
	"fmt"
	"math"
)

// Vertex2 ...
type Vertex2 struct {
	X, Y float64
}

// Abs ...
func (v Vertex2) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

// Scale ...
func (v *Vertex2) Scale(f float64) {
	v.X = v.X * f
	v.Y = v.Y * f
}

// MyFloat ...
type MyFloat float64

func methods() {
	v := Vertex2{3, 4}
	fmt.Println(v.Abs())
	// x := Scale(&v, 10)
}

// Methods does ..
func Methods() {
	fmt.Println("*** Methods ***")
	fmt.Println("")

	methods()
}
