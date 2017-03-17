pub fn raw_pointers() {
    println!("***Raw Pointers***");

    basics();
    references_and_raw_ptrs();

    println!("");
}

fn basics() {
    let x = 5;
    let raw_ptr = &x as *const i32;

    let mut y = 10;
    let raw_mut_ptr = &mut y as *mut i32;

    unsafe {
        println!("raw_ptr to x: {}", *raw_ptr);
        println!("raw_mut_ptr to y: {}", *raw_mut_ptr);
    }
}

fn references_and_raw_ptrs() {
    // Explicit cast:
    let i: u32 = 1;
    let p_imm: *const u32 = &i as *const u32;

    // implicit coercion
    let mut m: u32 = 2;
    let p_mut: *mut u32 = &mut m;

    unsafe {
        let ref_imm: &u32 = &*p_imm;
        let ref_mut: &mut u32 = &mut *p_mut;

        println!("ref_imm: {}", ref_imm);
        println!("ref_mut: {}", ref_mut);
    }
}
