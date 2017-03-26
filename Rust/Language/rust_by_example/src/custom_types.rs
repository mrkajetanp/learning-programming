#![allow(dead_code, unused_variables)]

pub fn custom_types() {
    println!("***Custom Types***");

    structures();
    enums();
    uses();
    c_like();
    linked_list();
    constants();

    println!("");
}

// a unit struct
struct _Nil;

// a tuple struct
struct Pair(i32, f32);

// a struct with two fields
#[derive(Debug)]
struct Point {
    x: f32,
    y: f32,
}

// nested structs
#[derive(Debug)]
struct Rectangle {
    p1: Point,
    p2: Point,
}

fn rect_area(r: &Rectangle) -> f32 {
    (r.p1.x - r.p2.x).abs() * (r.p1.y - r.p2.y).abs()
}

fn square(p: &Point, h: f32) -> Rectangle {
    Rectangle {
        p1: Point { x: p.x, y: p.y },
        p2: Point { x: p.x+h, y: p.y+h },
    }
}

fn structures() {
    let point: Point = Point { x: 0.3, y: 0.4 };

    println!("point coordinates: ({}, {})", point.x, point.y);

    // destructure the point
    let Point { x: my_x, y: my_y } = point;

    println!("point coords: {}, {}", my_x, my_y);

    let _rectangle = Rectangle {
        // struct instantiation is an expression too
        p1: Point { x: my_y, y: my_x },
        p2: point,
    };

    // instantiate a unit struct
    let _nil = _Nil;

    // instantiate a tuple struct
    let pair = Pair(1, 0.1);

    // access the fields of a tuple struct
    println!("pair contains {:?} and {:?}", pair.0, pair.1);

    // destructure a tuple struct
    let Pair(integer, decimal) = pair;

    println!("pair contains {:?} and {:?}", integer, decimal);

    let rect_one = Rectangle {
        p1: Point { x: 1.0, y: 3.0 },
        p2: Point { x: 3.0, y: 5.0 },
    };

    println!("area of my rect is: {}", rect_area(&rect_one));

    let my_point = Point { x: 1.0, y: 3.0 };
    let new_square = square(&my_point, 4.0);

    println!("square: {:?}", new_square);

    println!("new area is: {}", rect_area(&new_square));
}

enum Person {
    // an 'enum' may be 'unit-like'
    Engineer,
    Scientist,
    // like tuple structs
    Height(i32),
    Weight(i32),
    // or like structs,
    Info { name: String, height: i32 },
}

// function taking Person as an argument
fn inspect(p: Person) {
    // usage of an 'enum' must cover all cases
    // so a match is used
    match p {
        Person::Engineer => println!("Is an engineer!"),
        Person::Scientist => println!("Is a scientist!"),
        // Destructure i from inside the enum
        Person::Height(i) => println!("Has a height of {}", i),
        Person::Weight(i) => println!("Has a weight of {}", i),
        // Destructure 'Info' into name and height
        Person::Info { name, height } => {
            println!("{} is {} tall!", name, height);
        },
    }
}

fn enums() {
    let person = Person::Height(18);
    let amira = Person::Weight(10);
    // to_owned creates an owned 'String' from a string slice
    let dave = Person::Info { name: "Dave".to_owned(), height: 72 };
    let rebecca = Person::Scientist;
    let rohan = Person::Engineer;

    inspect(person);
    inspect(amira);
    inspect(dave);
    inspect(rebecca);
    inspect(rohan);
}

enum Status {
    Rich,
    Poor,
}

enum Work {
    Civilian,
    Soldier,
}

fn uses() {
    use self::Status::{Poor, Rich};
    use self::Work::*;

    // just like Status::Poor
    let status = Poor;
    let work = Civilian;

    match status {
        // lack of scoping because of the explicit use above
        Rich => println!("The rich have lots of money!"),
        Poor => println!("The poor have no money."),
    }

    match work {
        Civilian => println!("Civilians work!"),
        Soldier => println!("Soldiers fight!"),
    }
}

// implicit descriminator (starts at 0)
enum Number {
    Zero,
    One,
    Two,
}

// explicit descriminator
enum Color {
    Red = 0xff0000,
    Green = 0x00ff00,
    Blue = 0x0000ff,
}

fn c_like() {
    // enums can be cast as integers
    println!("zero is {}", Number::Zero as i32);
    println!("one is {}", Number::One as i32);

    println!("roses are #{:06x}", Color::Red as i32);
    println!("violets are #{:06x}", Color::Blue as i32);
}

// linked list implementation using enums
use self::List::*;

enum List {
    // Cons: Tuple struct with an element and a pointer to the next node
    Cons(u32, Box<List>),
    // Nil: node ending the linked list
    Nil,
}

impl List {
    // create new linked list
    fn new() -> List {
        Nil
    }

    // consume a list, return list with a new element at its front
    fn prepend(self, elem: u32) -> List {
        Cons(elem, Box::new(self))
    }

    // return the length
    fn len(&self) -> u32 {
        // self is a &List
        // match on *self (List)
        match *self {
            // can't take ownership of tail because self is borrowed
            // taking reference instead
            Cons(_, ref tail) => 1 + tail.len(),
            // base case, empty list has 0 length
            Nil => 0
        }
    }

    // return representation of the list as a (heap allocated) String
    fn stringify(&self) -> String {
        match *self {
            Cons(head, ref tail) => {
                // format! it's just like print! but returns a String
                format!("{}, {}", head, tail.stringify())
            },
            Nil => {
                format!("Nil")
            },
        }
    }
}

fn linked_list() {
    println!("");
    println!("--- linked list ---");

    let mut list = List::new();

    list = list.prepend(1);
    list = list.prepend(2);
    list = list.prepend(3);

    // show the final state of the list
    println!("linked list has length: {}", list.len());
    println!("{}", list.stringify());
}

// Globals are declared outside all other scopes
static LANGUAGE: &'static str = "Rust";
const THRESHOLD: i32 = 10;

fn is_big(n: i32) -> bool {
    // access constant in some function
    n > THRESHOLD
}

fn constants() {
    let n = 16;

    // access constant in the "main" thread
    println!("This is {}", LANGUAGE);
    println!("The threshold is {}", THRESHOLD);
    println!("{} is {}", n, if is_big(n) { "big" } else { "small" });

    // cannot modify a constant
    // THRESHOLD = 5;
}
