#![allow(unused_variables, dead_code, unused_assignments)]
pub fn primitives() {
    println!("***Primitives***");

    basics();
    literals_and_operators();
    tuples();
    arrays_and_slices();

    println!("");
}

fn basics() {
    let logical: bool = true;

    let a_float: f64 = 1.0; // regular annotation
    let an_integer = 5i32; // suffix annotation

    // default values
    let default_float = 3.0; // f64
    let default_int = 7; // i32

    let mut mutable = 12; // mutable i32

    mutable = 7;

    println!("Mutable: {}", mutable);
}

fn literals_and_operators() {
    println!("1 + 2 = {}", 1u32 + 2);

    println!("1 - 2 = {}", 1i32-2);

    // Short-circuiting boolean logic
    println!("true AND false is {}", true && false);
    println!("true OR false is {}", true || false);
    println!("NOT true is {}", !true);

    // bitwise operations
    println!("0011 AND 0101 is {:04b}", 0b0011u32 & 0b0101);
    println!("0011 OR 0101 is {:04b}", 0b0011u32 | 0b0101);
    println!("0011 XOR 0101 is {:04b}", 0b0011u32 ^ 0b0101);
    println!("1 << 5 is {}", 1u32 << 5);
    println!("0x80 >> 2 is 0x{:x}", 0x80u32 >> 2);

    // use underscores to improve readability
    println!("One million is {}", 1_000_000u32);
}

fn reverse(pair: (i32, bool)) -> (bool, i32) {
    let (integer, boolean) = pair;

    (boolean, integer)
}

use std::fmt;

#[derive(Debug)]
struct Matrix(f32, f32, f32, f32);

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "( {} {} )\n", self.0, self.1)?;
        write!(f, "( {} {} )", self.2, self.3)
    }
}

fn transpose(m: Matrix) -> Matrix {
    Matrix(m.0, m.2, m.1, m.3)
}

fn tuples() {
    let long_tuple = (1u8, 2u16, 3u32, 4u64,
                      -1i8, -2i16, -3i32, -4i64,
                      0.1f32, 0.2f64,
                      'a', true);

    println!("long tuple first value: {}", long_tuple.0);
    println!("long tuple second value: {}", long_tuple.1);

    // tuples can be nested
    let tuple_of_tuples = ((1u8, 2u16, 2u32), (4u64, -1i8), -2i16);

    // tuples are printable
    println!("tuple of tuples: {:?}", tuple_of_tuples);

    let pair = (1, true);
    println!("pair is {:?}", pair);

    println!("the reversed pair is: {:?}", reverse(pair));

    // for one element tuples, comma is required
    println!("one element tuple: {:?}", (5u32,));
    println!("just an integer: {:?}", (5u32));

    // destructuring tuples
    let tuple = (1, "hello", 4.5, true);

    let (a, b, c, d) = tuple;
    println!("Tuple elements: {:?} {:?} {:?} {:?}", a, b, c, d);

    let matrix = Matrix(1.1, 1.2, 2.1, 2.2);
    println!("Matrix:\n{}", matrix);
    println!("Transpose:\n{}", transpose(matrix));
}

use std::mem;

// this function borrows a slice
fn analyze_slice(slice: &[i32]) {
    println!("the first element of the slice: {}", slice[0]);
    println!("the slice has {} elements", slice.len());
}

fn arrays_and_slices() {
    // fixed-size array
    let xs: [i32; 5] = [1, 2, 3, 4, 5];

    // all elements can be initialized to the same value
    let ys: [i32; 500] = [0; 500];

    // indexing starts at 0
    println!("first: {}", xs[0]);
    println!("second: {}", xs[1]);

    // len returns the size
    println!("array size: {}", xs.len());

    // arrays are stack allocated
    println!("array occupies {} bytes", mem::size_of_val(&xs));
    analyze_slice(&xs);

    // slices can point to a section of an array
    analyze_slice(&ys[1..4]);

    // out of bounds indexing panics
    // println!("{}", xs[5]);
}
