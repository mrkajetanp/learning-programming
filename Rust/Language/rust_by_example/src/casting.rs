#![allow(overflowing_literals, unused_variables)]
pub fn casting() {
    println!("***Casting***");

    basics();
    literals();
    inference();
    aliases();

    println!("");
}

fn basics() {
    let decimal = 65.4321_f32;

    let integer = decimal as u8;
    let character = integer as char;

    println!("Casting {} -> {}, {}", decimal, integer, character);

    println!("1000 as u16 = {}", 1000 as u16);
    println!("1000 as u8 = {}", 1000 as u8);

    println!("-2 as u8 is {}", (-2i8) as u8);

    // casting positive number to u8 is just like %256
    println!("1000 mod 256 is {}", 1000 % 256);

    println!("128 as i16 is {}", 128 as i16);
    println!("128 as i8 is {}", 128 as i8);

    println!("1000 as i8 is {}", 1000 as i8);
    println!("232 as i8 is {}", 232 as i8);
}

use std::mem;

fn literals() {
    let x = 1u8;
    let y = 2_u32;
    let z = 3_f32;

    let i = 1;
    let f = 1.0;

    println!("size of x in bytes: {}", mem::size_of_val(&x));
    println!("size of y in bytes: {}", mem::size_of_val(&y));
    println!("size of z in bytes: {}", mem::size_of_val(&z));
    println!("size of i in bytes: {}", mem::size_of_val(&i));
    println!("size of f in bytes: {}", mem::size_of_val(&f));
}

fn inference() {
    let elem = 5_u8;

    // now it's just a Vec<_>
    let mut vec = Vec::new();

    // now compiler infers the type - it's Vec<u8>
    vec.push(elem);

    println!("{:?}", vec);
}

type NanoSecond = u64;
type Inch = u64;

fn aliases() {
    let nanoseconds: NanoSecond = 5 as u64;
    let inches: Inch = 6 as u64;

    // they're just aliases, not new types
    println!("{} nanoseconds + {} inches = {} unit?",
             nanoseconds, inches, nanoseconds + inches);
}
