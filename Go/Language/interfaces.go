package main

import (
	"fmt"
	"math"
)

type Abser interface {
	Abs() float64
}

type IMyFloat float64

func (f IMyFloat) Abs() float64 {
	if f < 0 {
		return float64(-f)
	}
	return float64(f)
}

type IVertex struct {
	X, Y float64
}

func (v *IVertex) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

func Interfaces() {
	fmt.Println("*** Interfaces ***");
	var a Abser

	f := IMyFloat(-math.Sqrt2)
	v := IVertex{3, 4}

	a = f // MyFloat implements Abser
	a = &v // *Vertex implements Abser

	// a = v

	fmt.Println(a.Abs())
}
