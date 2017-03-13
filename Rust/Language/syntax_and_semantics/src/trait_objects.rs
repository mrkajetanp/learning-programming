pub fn trait_objects() {
    println!("***Trait Objects****");

    static_dispatch();
    dynamic_dispatch();

    // only traits that are object-safe can be made into trait objects

    println!("");
}

trait Foo {
    fn method(&self) -> String;
}

impl Foo for u8 {
    fn method(&self) -> String {
        format!("u8: {}", *self)
    }
}

impl Foo for String {
    fn method(&self) -> String {
        format!("string: {}", *self)
    }
}

fn do_something<T: Foo>(x: T) {
    println!("static: {}", x.method());
}

fn static_dispatch() {
    let x: u8 = 5;
    let y = "Hello".to_string();

    do_something(x);
    do_something(y);
}

// function takes a trait object
fn do_something_dyn(x: &Foo) {
    println!("dynamic: {}", x.method());
}

fn dynamic_dispatch() {
    let x: u8 = 5;
    let y = "test".to_string();
    // by casting
    do_something_dyn(&x as &Foo);
    // by coercing
    do_something_dyn(&y);
}

#[allow(dead_code)]
struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

#[allow(dead_code)]
struct FooVtable {
    destructor: fn(*mut ()),
    size: usize,
    align: usize,
    method: fn(*const ()) -> String,
}

#[allow(dead_code)]
fn call_method_on_u8(x: *const ()) -> String {
    // compiler guarantees that this function is only called with 'x' pointing to a u8
    let byte: &u8 = unsafe { &*(x as *const u8) };
    byte.method()
}

// static Foo_for_u8_vtable: FooVtable = FooVtable {
//     destructor: /* compiler stuff */,
//     size: 1,
//     align: 1,
//     method: call_method_on_u8 as fn(*const ()) -> String,
// };

#[allow(dead_code)]
fn call_method_on_string(x: *const ()) -> String {
    // compiler guarantees that this function is only called with 'x' pointing to a String
    let string: &String = unsafe { &*(x as *const String) };

    string.method()
}
