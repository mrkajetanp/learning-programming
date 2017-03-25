#![allow(dead_code)]
pub fn formatted_print() {
    println!("***Formatted Print***");

    basics();
    debug();
    display();
    testcase_list();
    formatting();

    println!("");
}

fn basics() {
    println!("{} days", 31);

    // positional arguments
    println!("{0}, this is {1}. {1}, this is {0}", "Alice", "Bob");

    // named arguments
    println!("{subject} {verb} {object}", object="a dog", subject="brown fox", verb="jumps over");

    // special formatting
    println!("decimal: {} or binary: {:b}", 2, 2);

    // aligning text with specified width
    // "     1" 5 white spaces and a 1
    println!("{number:>width$}", number=1, width=6);

    // you can pad numbers with extra zeroes
    println!("{number:>0width$}", number=1, width=6);
}

#[derive(Debug)]
struct Structure(i32);

#[derive(Debug)]
struct Deep(Structure);

fn debug() {
    println!("{:?} months in a year.", 12);

    println!("{1:?} {0:?} is the {actor:?} name.", "Slater", "Christian", actor="actor's");

    println!("Now {:?} will print!", Structure(3));
}

use std::fmt;

#[derive(Debug)]
struct MinMax(i64, i64);

// implement Display for MinMax
impl fmt::Display for MinMax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.0, self.1)
    }
}

impl fmt::Binary for MinMax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:b},{:b})", self.0, self.1)
    }
}

#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64,
}

impl fmt::Display for Point2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "x: {}, y: {}", self.x, self.y)
    }
}

#[derive(Debug)]
struct Complex {
    real: f64,
    imag: f64,
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} + {}i", self.real, self.imag)
    }
}

fn display() {
    let minmax = MinMax(2, 14);

    println!("MinMax Display: {}", minmax);
    println!("MinMax Debug: {:?}", minmax);
    println!("MinMax Binary: {:b}", minmax);

    let big_range = MinMax(-300, 300);
    let small_range = MinMax(-3, 3);

    println!("The big range is {big} and the small is {small}", small=small_range, big=big_range);

    let point = Point2D { x: 3.3, y: 7.2 };

    println!("Point2D Display: {}", point);
    println!("Point2D Debug: {:?}", point);

    let complex = Complex { real: 3.3, imag: 7.2 };

    println!("Complex Display: {}", complex);
    println!("Complex Debug: {:?}", complex);
}

struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Extract the value using tuple indexing
        // and create reference to vec
        let vec = &self.0;

        write!(f, "[")?;

        // iterate over vec in v while enumerating the iteration
        // count in 'count'
        for (count, v) in vec.iter().enumerate() {
            // For every element except the first, add a comma
            // Use the ? operator or try! to return on errors
            if count != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", count, v)?;
        }

        // Close the bracket and return a fmt::Result value
        write!(f, "]")
    }
}

fn testcase_list() {
    let v = List(vec![1, 2, 3]);
    println!("List: {}", v);
}

struct City {
    name: &'static str,
    lat: f32,
    lon: f32,
}

impl fmt::Display for City {
    // 'f' is a buffer, method must write formatted string into it
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lat_c = if self.lat >= 0.0 { 'N' } else { 'S' };
        let lon_c = if self.lon >= 0.0 { 'E' } else { 'W' };

        // write! is like format! but writes into a buffer
        write!(f, "{}: {:.3}°{} {:.3}°{}", self.name,
               self.lat.abs(), lat_c,
               self.lon.abs(), lon_c)
    }
}

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RGB ({0}, {1}, {2}) 0x{0:02X}{1:02X}{2:02X}", self.red, self.green, self.blue)
    }
}

fn formatting() {
    let foo = 1234;
    println!("Decimal: {}", foo);
    println!("Hex: 0x{:X}", foo);
    println!("Oct: 0o{:o}", foo);
    println!("Bin: {:b}", foo);

    for city in [
        City { name: "Dublin", lat: 53.34778, lon: -6.259772 },
        City { name: "Oslo", lat: 59.95, lon: 10.75 },
        City { name: "Vancouver", lat: 49.25, lon: -123.1 },
    ].iter() {
        println!("{}", city);
    }

    for color in [
        Color { red: 128, green: 255, blue: 90 },
        Color { red: 0, green: 3, blue: 254 },
        Color { red: 0, green: 0, blue: 0 },
    ].iter() {
        println!("{}", color);
    }
}
