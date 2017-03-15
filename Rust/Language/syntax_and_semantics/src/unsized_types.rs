pub fn unsized_types() {
    println!("***Unsized Types***");

    one();

    println!("");
}

fn one() {
    // can only manipulate via a pointer
    // impl Foo for str
    // impl<T> Foo for [T]
}

// T may or may not be Sized
#[allow(dead_code)]
struct Foo<T: ?Sized> {
    f: T,
}
