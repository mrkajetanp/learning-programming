pub fn const_and_static() {
    println!("***Const and Static***");

    one();

    println!("");
}

#[allow(dead_code)]
fn one() {
    // lifetime of a program, inlined upon use, no fixed memory address
    const N: i32 = 5;

    // lifetime of a program, not inlined upon use, fixed memory location
    static N2: i32 = 5;

    // any reference stored in static has a 'static lifetime
    static NAME: &'static str = "Steve";

    // mutable & static, can cause memory unsafety
    static mut N3: i32 = 5;

    // accessing and mutating such a variable must be done in an unsafe block
    unsafe {
        N3 += 1;
        println!("N3: {}", N3);
    }

    // any type stored in a static must be Sync, and must not have a Drop implementation

    // one should almost always choose const over static
}
