pub fn casting() {
    println!("***Casting***");

    coercion();
    as_safe_cast();
    pointer_casts();
    transmute();

    println!("");
}

fn coercion() {
    // is implicit but can be spelled out with "as"
    // occurs in
    // let, const, static statements, fn call args, struct field init,
    // in function result

    // removing mutability from a reference
    // &mut T -> &T

    // remove mutability from a raw pointer
    // *mut T -> *const T

    // references -> raw pointers
    // &T -> *const T
    // &mut T -> *mut T

    // custom coercions may be defined using "Deref"
}

#[allow(unused_variables)]
fn as_safe_cast() {
    let x: i32 = 5;
    let y = x as i64;

    // there are 3 categories of safe cast
    // explicit coercions, casts between numeric types and pointer casts

    let one = true as u8;
    let at_sign = 64 as char;
    let two_hundred = -56i8 as u8;

    println!("{}, {}, {}", one, at_sign, two_hundred);
}

fn pointer_casts() {
    let a = 300 as *const char; // 'a' is a pointer to location 300
    let b = a as u32;

    println!("b: {}", b);
}

use std::mem;

fn transmute() {
    // transmute makes sure that the types are the same size
    unsafe {
        let a = [0u8, 1u8, 0u8, 0u8];
        let b = mem::transmute::<[u8; 4], u32>(a);
        println!("transmuted b: {}", b);

        let c: u32 = mem::transmute(a);
        println!("transmuted c: {}", c);
    }
}
