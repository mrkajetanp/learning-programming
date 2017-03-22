#![allow(dead_code, unused_variables)]
pub fn error_handling_library_traits() {
    println!("***Error Handling - Standard Library Traits");

    one();

    println!("");
}

// How Error trait is defined in a standard library
// use std::fmt::{Debug, Display};
// trait Error: Debug + Display {
//     // a short description of the error
//     fn description(&self) -> &str;
//     // the lower level cause of this error
//     fn cause(&self) -> Option<&Error> { None }
// }

use std::io;
use std::num;

#[derive(Debug)]
enum CliError {
    Io(io::Error),
    Parse(num::ParseIntError),
}

use std::error;
use std::fmt;

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // both underlying errors already impl 'Display'
            // so we refer to their implementations
            CliError::Io(ref err) => write!(f, "IO Error: {}", err),
            CliError::Parse(ref err) => write!(f, "Parse Error: {}", err),
        }
    }
}

impl error::Error for CliError {
    fn description(&self) -> &str {
        // both underlying errors already implement 'Error'
        // so we refer to their implementations
        match *self {
            CliError::Io(ref err) => err.description(),
            CliError::Parse(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            // Both of these implicitly cast 'err' from their concrete types
            // either &io::Error or &num::ParseIntError
            // to a trait object &Error
            // works because both error types implement 'Error'
            CliError::Io(ref err) => Some(err),
            CliError::Parse(ref err) => Some(err),
        }
    }
}

// the From trait

use std::error::Error;
// use std::io;
// use std::num;

fn one() {
    let string: String = From::from("foo");
    let bytes: Vec<u8> = From::from("foo");
    let cow: ::std::borrow::Cow<str> = From::from("foo");

    let io_err: io::Error = io::Error::last_os_error();
    let parse_err: num::ParseIntError = "not a number".parse::<i32>().unwrap_err();

    // patern for converting errors to the same type using the same function
    let err1: Box<Error> = From::from(io_err);
    let err2: Box<Error> = From::from(parse_err);
}

use std::fs::File;
use std::path::Path;
use std::io::Read;

fn file_double_even_better<P: AsRef<Path>>(file_path: P) -> Result<i32, Box<Error>> {
    let mut file = try!(File::open(file_path));
    let mut contents = String::new();
    try!(file.read_to_string(&mut contents));
    let n = try!(contents.trim().parse::<i32>());
    Ok(2 * n)
}

// Composing custom error types

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> CliError {
        CliError::Io(err)
    }
}

impl From<num::ParseIntError> for CliError {
    fn from(err: num::ParseIntError) -> CliError {
        CliError::Parse(err)
    }
}

fn file_double_best<P: AsRef<Path>>(file_path: P) -> Result<i32, CliError> {
    let mut file = try!(File::open(file_path));
    let mut contents = String::new();
    try!(file.read_to_string(&mut contents));
    let n: i32 = try!(contents.trim().parse());
    Ok(2 * n)
}
