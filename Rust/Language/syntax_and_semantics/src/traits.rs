use std;

pub fn traits() {
    println!("***Traits***", );

    traits_one();
    trait_bounds_on_generic_structs();
    traits_for_any_type();
    default_methods();
    deriving();

    println!("");
}

trait HasArea {
    fn area(&self) -> f64;
}

#[allow(dead_code)]
struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}

#[allow(dead_code)]
struct Square {
    x: f64,
    y: f64,
    side: f64,
}

impl HasArea for Square {
    fn area(&self) -> f64 {
        self.side * self.side
    }
}

fn print_area<T: HasArea>(shape: T) {
    println!("This shape has an area of {}", shape.area());

}

fn traits_one() {
    let c = Circle { x: 0.0f64, y: 0.0f64, radius: 1.0f64 };
    let s = Square { x: 0.0f64, y: 0.0f64, side: 2.0f64 };

    print_area(c);
    print_area(s);
}

#[allow(dead_code)]
struct Rectangle<T> {
    x: T,
    y: T,
    width: T,
    height: T,
}

impl<T: PartialEq> Rectangle<T> {
    fn is_square(&self) -> bool {
        self.width == self.height
    }
}

fn trait_bounds_on_generic_structs() {
    let mut r = Rectangle { x: 0, y: 0, width: 47, height: 47 };

    assert!(r.is_square());
    r.height = 42;
    assert!(!r.is_square());
}

trait ApproxEqual {
    fn approx_equal(&self, other: &Self) -> bool;
}

impl ApproxEqual for f32 {
    fn approx_equal(&self, other: &Self) -> bool {
        (self - other).abs() <= std::f32::EPSILON
    }
}

// either the trait of the type must be defined by you

fn traits_for_any_type() {
    println!("approx_equal: {}", 1.0.approx_equal(&1.000000001));
}

// multiple trait bounds
use std::fmt::Debug;

#[allow(dead_code)]
fn foo<T: Clone + Debug>(x: T) {
    x.clone();
    println!("x: {:?}", x);
}

// where clause
#[allow(dead_code)]
fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug {
    x.clone();
    y.clone();
    println!("y: {:?}", y);
}

trait ConvertTo<Output> {
    fn convert(&self) -> Output;
}

impl ConvertTo<i64> for i32 {
    fn convert(&self) -> i64 {
        *self as i64
    }
}


#[allow(dead_code)]
// can be called with T == i32
fn normal<T: ConvertTo<i64>>(x: &T) -> i64 {
    x.convert()
}

#[allow(dead_code)]
// can be called with T == i64
fn inverse<T>(x: i32) -> T
        // This is using ConvertTo as if it were "ConvertTo<i64>".
        where i32: ConvertTo<T> {
    x.convert()
}

trait Foo {
    fn is_valid(&self) -> bool;
    // default function, opposite of is_valid
    fn is_invalid(&self) -> bool { !self.is_valid() }
}

struct UseDefault;

impl Foo for UseDefault {
    fn is_valid(&self) -> bool {
        println!("Called UseDefault.is_valid.");
        true
    }
}

struct OverrideDefault;

impl Foo for OverrideDefault {
    fn is_valid(&self) -> bool {
        println!("Called OverrideDefault.is_valid.");
        true
    }

    fn is_invalid(&self) -> bool {
        println!("Called OverrideDefault.is_invalid!");
        true
    }
}

fn default_methods() {
    let default = UseDefault;
    assert!(!default.is_invalid());

    let over = OverrideDefault;
    assert!(over.is_invalid());
}

// Inheritance
// Implementors of FooBar must also implement Foo

trait Foo2 {
    fn foo(&self);
}

trait FooBar : Foo2 {
    fn foobar(&self);
}

#[allow(dead_code)]
struct Baz;

impl Foo2 for Baz {
    fn foo(&self) {
        println!("foo");
    }
}

impl FooBar for Baz {
    fn foobar(&self) {
        println!("foobar");
    }
}

// Foo derives Debug trait
#[derive(Debug)]
struct Foo3;

fn deriving() {
    println!("{:?}", Foo3);
}
