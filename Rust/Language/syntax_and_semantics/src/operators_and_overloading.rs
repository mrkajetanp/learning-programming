pub fn operators_and_overloading() {
    println!("***Operators and Overloading***");

    one();
    generic_structs();

    println!("");
}

use std::ops::Add;

#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}

#[allow(unused_variables)]
impl Add<i32> for Point {
    type Output = f64;

    // doesn't make sense but works
    fn add(self, rhs: i32) -> f64 {
        (self.x + rhs) as f64 + 0.1
    }
}

fn one() {
    let p1 = Point { x: 1, y: 0 };
    let p2 = Point { x: 2, y: 3 };

    let p3 = p1 + p2;
    println!("P3: {:?}", p3);

    let p: Point = Point { x: 1, y: 2 };
    let x: f64 = p + 2i32;

    println!("x: {}", x);
}

// operator traits in generic structs

use std::ops::Mul;

trait HasArea<T> {
    fn area(&self) -> T;
}

#[allow(dead_code)]
struct Square<T> {
    x: T,
    y: T,
    side: T,
}

impl<T> HasArea<T> for Square<T> where T: Mul<Output=T> + Copy {
    fn area(&self) -> T {
        self.side * self.side
    }
}

fn generic_structs() {
    let s = Square {
        x: 0.0f64,
        y: 0.0f64,
        side: 12.0f64,
    };

    println!("Area of s: {}", s.area());
}
