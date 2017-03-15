use std::result;

pub fn type_aliases() {
    println!("***Type Aliases***");

    one();

    println!("");
}

#[allow(dead_code, unused_variables)]
pub fn one() {
    type Name = String;
    let x: Name = "Cajetan".to_string();

    let x: i32 = 5;
    let y: i64 = 7;

    // Won't compile because of mismatched type
    // if x == y {
    //     println!("Equal!");
    // }

    type Num = i32;

    let x: i32 = 5;
    let y: Num = 5;

    // OK because Num is just an alias, not a different type
    if x == y {
        println!("{} and {} are equal!", x, y);
    }
}

#[allow(dead_code)]
enum ConcreteError {
    Foo,
    Bar,
}

// specialized version of Result type which always has ConcreteError for the E part of Result<T, E>
#[allow(dead_code)]
type Result<T> = result::Result<T, ConcreteError>;


