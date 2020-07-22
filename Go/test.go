package main

import (
	"fmt"
)

func vartest(names ...string) {
	if len(names) == 0 {
		fmt.Println("you");
	} else {
		fmt.Println(names[0]);
	}
}

func main() {
	fmt.Println("hello there");
	vartest();
	vartest("Something");
	vartest("Sth", "Else");
}
