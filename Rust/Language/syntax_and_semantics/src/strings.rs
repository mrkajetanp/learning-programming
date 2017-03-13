pub fn strings() {
    println!("***Strings***");

    strs();
    strings_s();
    indexing();
    slicing();
    concatenation();

    println!("");
}

#[allow(dead_code, unused_variables)]
fn strs() {
    let greetings = "hello, world!"; // greeting: &'static str

    let s = "foo
     bar"; // with \n
    assert_eq!("foo\n     bar", s);

    let s2 = "foo\
              bar";
    assert_eq!("foobar", s2);
}

fn takes_slice(slice: &str) {
    println!("got: {}", slice);
}

// use std::net::TcpStream;

fn strings_s() {
    let mut s = "Hello".to_string(); // mut s: String
    println!("{}", s);
    assert_eq!("Hello".to_string(), s);

    s.push_str(", world");
    println!("{}", s);
    assert_eq!("Hello, world".to_string(), s);

    // Strings with coerce into &str
    let s = "Hello".to_string();
    takes_slice(&s);

    // TcpStream::connect("192.168.0.1:3000"); // parameter is &str
    // let addr_string = "192.168.0.1:3000".to_string();
    // TcpStream::connect(&*addr_string); // explicit conversion, involves allocating memory!
}

fn indexing() {
    // you can't index with []

    let text = "aoeuid";

    for b in text.as_bytes() {
        print!("{} ", b);
    }
    println!("");

    for c in text.chars() {
        print!("{} ", c);
    }
    println!("");

    let c1 = text.chars().nth(1); // kinda like text[1]
    println!("c1: {:?}", c1);
}

fn slicing() {
    // these are byte offsets, not character offsets
    let dog = "hachiko";
    let hachi = &dog[0..5];

    println!("hachi: {}", hachi);
}

fn concatenation() {
    // you can concatenate &str to the String
    let hello = "Hello ".to_string();
    let world = "world!";
    let hello_world = hello + world;
    println!("hello_world: {}", hello_world);

    // to concatenate two Strings, you need to use an &
    let hello = "Hello ".to_string();
    let world = "world!".to_string();
    let hello_world = hello + &world;
    println!("hello_world: {}", hello_world);

    // &String automatically coerces to &str
    // It's called Deref coercions
}
