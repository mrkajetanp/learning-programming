#![allow(unused_variables)]

pub fn stack_and_heap() {
    one();
}

fn foo(x: &i32) {
    let y = 10;
    let z = &y;

    baz(z);
    bar(x, z);
}

fn bar(a: &i32, b: &i32) {
    let c = 5;
    let d = Box::new(5);
    // e = address of d (on the heap)
    let e = &d;

    baz(e);
}

fn baz(f: &i32) {
    let g = 100;
}

fn one() {
    // stack allocation
    let h = 3;
    // heap allocation
    let i = Box::new(20);
    // j = address of h (on the stack)
    let j = &h;

    foo(j);
}

