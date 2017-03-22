#![allow(dead_code)]

pub fn error_handling_multiple_types() {
    println!("***Error Handling - Multiple Types***");

    one();

    println!("");
}

use std::env;

fn double_arg(mut argv: env::Args) -> Result<i32, String> {
    argv.nth(1)
        .ok_or("Please give at least one argument".to_owned())
        .and_then(|arg| arg.parse::<i32>().map_err(|err| err.to_string()))
        .map(|n| 2 * n)
}

use std::fs::File;
use std::io::Read;
use std::path::Path;

fn file_double_panics<P: AsRef<Path>>(file_path: P) -> i32 {
    let mut file = File::open(file_path).unwrap(); // err 1
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap(); // err 2
    let n: i32 = contents.trim().parse().unwrap(); // err 3
    2 * n
}

fn file_double_handled<P: AsRef<Path>>(file_path: P) -> Result<i32, String> {
    File::open(file_path)
        .map_err(|err| err.to_string())
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| err.to_string())
                .map(|_| contents)
        })
        .and_then(|contents| {
            contents.trim().parse::<i32>()
                .map_err(|err| err.to_string())
        })
        .map(|n| 2 * n)
}

fn file_double_early_return<P: AsRef<Path>>(file_path: P) -> Result<i32, String> {
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => return Err(err.to_string()),
    };

    let mut contents = String::new();

    if let Err(err) = file.read_to_string(&mut contents) {
        return Err(err.to_string());
    }

    let n: i32 = match contents.trim().parse() {
        Ok(n) => n,
        Err(err) => return Err(err.to_string()),
    };

    Ok(2 * n)
}

// try! macro abstracts away explicit match and return
fn file_double_try<P: AsRef<Path>>(file_path: P) -> Result<i32, String> {
    let mut file = try!(File::open(file_path).map_err(|err| err.to_string()));
    let mut contents = String::new();
    try!(file.read_to_string(&mut contents).map_err(|err| err.to_string()));
    let n = try!(contents.trim().parse::<i32>().map_err(|err| err.to_string()));
    Ok(2 * n)
}

// defining an own error type

use std::io;
use std::num;

// all types should probably derive 'Debug'
#[derive(Debug)]
enum CliError {
    Io(io::Error),
    Parse(num::ParseIntError),
}

fn file_double_custom_error<P: AsRef<Path>>(file_path: P) -> Result<i32, CliError> {
    let mut file = try!(File::open(file_path).map_err(CliError::Io));
    let mut contents = String::new();
    try!(file.read_to_string(&mut contents).map_err(CliError::Io));
    let n: i32 = try!(contents.trim().parse().map_err(CliError::Parse));
    Ok(2 * n)
}

fn one() {
    match file_double_custom_error("foobar") {
        Ok(n) => println!("{}", n),
        Err(err) => println!("Error: {:?}", err),
    }
}
