#![allow(dead_code, unused_variables)]
pub fn error_handling_basics() {
    println!("***Error Handling Basics***");

    basics();
    unwrapping();
    composing_option_values();

    println!("");
}

// Bad error handling
fn guess(n: i32) -> bool {
    if n < 1 || n > 10 {
        panic!("Invalid number {}", n);
    }
    n == 5
}

use std::env;

// still bad, unwrap panics if given invalid arguments
fn from_env() {
    let mut argv = env::args();
    let arg: String = argv.nth(1).unwrap(); // potential error 1
    let n: i32 = arg.parse().unwrap(); // potential error 2
    println!("{}", 2*n);
}

fn basics() {
    guess(5);
}

// just like Option<T>
enum MyOption<T> {
    None,
    Some(T),
}

// how unwrap() is actually implemented
impl<T> MyOption<T> {
    fn unwrap(self) -> T {
        match self {
            MyOption::Some(val) => val,
            MyOption::None => panic!("Called Option::unwrap() on a 'None' value"),
        }
    }
}

// Searches haystack for the Unicode character needle.
// If one is found, returns its byte offset as Some(offset)
// Otherwise, None is returned.
fn find(haystack: &str, needle: char) -> Option<usize> {
    for (offset, c) in haystack.char_indices() {
        if c == needle {
            return Some(offset);
        }
    }
    None
}

fn unwrapping() {
    let file_name = "foobar.rs";
    match find(file_name, '.') {
        Some(i) => println!("File extension: {}", &file_name[i+1..]),
        None => println!("No file extension found."),
    }

}

// Returns the extension of a given file name, or None
// There is an extension method in a standard library for it
fn extension_explicit(file_name: &str) -> Option<&str> {
    match find(file_name, '.') {
        None => None,
        Some(i) => Some(&file_name[i+1..]),
    }
}

fn my_map<F, T, A>(option: Option<T>, f: F) -> Option<A> where F: FnOnce(T) -> A {
    match option {
        None => None,
        Some(value) => Some(f(value)),
    }
}

// the same, just using map this time
fn extension(file_name: &str) -> Option<&str> {
    find(file_name, '.').map(|i| &file_name[i+1..])
}

fn unwrap_or<T>(option: Option<T>, default: T) -> T {
    match option {
        None => default,
        Some(value) => value,
    }
}

fn composing_option_values() {
    assert_eq!(extension("foobar.csv").unwrap_or("rs"), "csv");
    assert_eq!(extension("foobar").unwrap_or("rs"), "rs");
}

fn file_path_ext_explicit(file_path: &str) -> Option<&str> {
    match file_name(file_path) {
        None => None,
        Some(name) => match extension(name) {
            None => None,
            Some(ext) => Some(ext),
        }
    }
}

fn and_then<F, T, A>(option: Option<T>, f: F) -> Option<A>
    where F: FnOnce(T) -> Option<A> {

    match option {
        None => None,
        Some(value) => f(value),
    }
}

fn file_path_ext(file_path: &str) -> Option<&str> {
    file_name(file_path).and_then(extension)
}

fn file_name(file_path: &str) -> Option<&str> {
    // Implementation elided
    unimplemented!();
}

// the result type

enum MyResult<T, E> {
    Ok(T),
    Err(E),
}

type MyOption2<T> = MyResult<T, ()>;

impl<T, E: ::std::fmt::Debug> MyResult<T, E> {
    fn unwrap(self) -> T {
        match self {
            MyResult::Ok(val) => val,
            MyResult::Err(err) =>
                panic!("called MyResult::unwrap() on an `err` value: {:?}", err),
        }
    }
}

// Parsing integers

use std::num::ParseIntError;

fn double_number_explicit(number_str: &str) -> Result<i32, ParseIntError> {
    match number_str.parse::<i32>() {
        Ok(n) => Ok(2 * n),
        Err(err) => Err(err),
    }
}

fn double_number(number_str: &str) -> Result<i32, ParseIntError> {
    number_str.parse::<i32>().map(|n| 2 * n)
}

fn parsing_integers() {
    match double_number_explicit("10") {
        Ok(n) => assert_eq!(n, 20),
        Err(err) => println!("Error: {:?}", err),
    }

    match double_number("10") {
        Ok(n) => assert_eq!(n, 20),
        Err(err) => println!("Error: {:?}", err),
    }
}

// The Result type alias idiom
use std::result;

// constrained error to always be ParseIntError for MyResult2<T>
type MyResult2<T> = result::Result<T, ParseIntError>;
