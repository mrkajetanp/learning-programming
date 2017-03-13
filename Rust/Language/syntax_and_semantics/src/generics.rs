use std;

pub fn generics() {
    println!("***Generics***");

    generics_basics();
    generic_functions();
    generic_structs();
    resolving_ambiguities();

    println!("");
}

#[allow(dead_code)]
enum Option<T> {
    Some(T),
    None,
}

#[allow(dead_code)]
enum Result<A, Z> {
    Ok(A),
    Err(Z),
}

#[allow(unused_variables)]
fn generics_basics() {
    let x: Option<i32> = Option::Some(5);
    let y: Option<f64> = Option::Some(5.0f64);
    let z: Result<i32, f64> = Result::Ok(18);
}

#[allow(unused_variables)]
fn takes_anything<T>(x: T) {
    // something with x
}

#[allow(unused_variables)]
fn takes_two_the_same_things<T>(x: T, y: T) {
    // ...
}

#[allow(unused_variables)]
fn takes_two_things<T, U>(x: T, y: U) {
    // ...
}

fn generic_functions() {
    takes_anything(7);
    takes_anything(7.5);
    takes_two_the_same_things(7, 7);
    takes_two_things(7, 7.5);
}

#[allow(dead_code)]
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn swap(&mut self) {
        std::mem::swap(&mut self.x, &mut self.y);
    }
}

#[allow(unused_variables)]
fn generic_structs() {
    let int_origin = Point { x: 0, y: 0 };
    let mut float_origin = Point { x: 1.1, y: 2.2 };

    println!("Point: x: {}, y: {}", float_origin.x, float_origin.y);
    assert_eq!(1.1, float_origin.x);
    assert_eq!(2.2, float_origin.y);

    float_origin.swap();
    println!("Point: x: {}, y: {}", float_origin.x, float_origin.y);
    assert_eq!(2.2, float_origin.x);
    assert_eq!(1.1, float_origin.y);
}

fn resolving_ambiguities() {
    let mut v = Vec::new(); // this alone won't compile
    v.push(true); // bool value inferred at this point
    println!("v: {:?}", v);

    let v2: Vec<bool> = Vec::new(); // type annotated here, can compile now
    println!("v2: {:?}", v2);

    // by turbofish syntax
    let v3 = Vec::<bool>::new();
    println!("v3: {:?}", v3);
}
