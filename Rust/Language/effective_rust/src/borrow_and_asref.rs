use std::collections::HashMap;

pub fn borrow_asref() {
    println!("***Borrow and AsRef***");

    one();

    println!("");
}

use std::borrow::Borrow;
use std::fmt::Display;

fn foo<T: Borrow<i32> + Display>(a: T) {
    println!("a is borrowed: {}", a);
}

fn bar<T: AsRef<str>>(s: T) {
    let slice = s.as_ref();
    println!("reference to slice: {}", slice);
}

fn one() {
    // how it works in practice (used for HashMap)

    let mut map = HashMap::new();
    map.insert("Foo".to_string(), 42);

    assert_eq!(map.get("Foo"), Some(&42));
    println!("map.get(Foo) = {}", map.get("Foo").unwrap());

    let mut i = 5;

    foo(&i);
    foo(&mut i);

    let slice_one: &str = "testme";
    bar(slice_one);
    bar(slice_one);
}
