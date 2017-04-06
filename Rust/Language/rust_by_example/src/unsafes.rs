pub fn unsafes() {
    println!("***Unsafes***");

    raw_ptrs();
    transmutes();

    println!("");
}

fn raw_ptrs() {
    let raw_p: *const u32 = &10;

    // dereferencing raw pointers
    unsafe {
        assert!(*raw_p == 10);
    }
}

fn transmutes() {
    use std;
    let u: &[u8] = &[49, 50, 51];

    unsafe {
        assert!(u == std::mem::transmute::<&str, &[u8]>("123"));
    }
}
