pub fn ptr() {
    functions();
}

use std::ptr;

unsafe fn from_buf_raw<T>(ptr: *const T, elems: usize) -> Vec<T> {
    let mut result = Vec::with_capacity(elems);
    result.set_len(elems);

    // just like C's memmove
    ptr::copy(ptr, result.as_mut_ptr(), elems);
    result
}

fn functions() {
    let five = 5;
    let other_five = 5;

    assert!(&five == &five);
    assert!(&five == &other_five);

    // compares addresses
    assert!(ptr::eq(&five, &five));
    assert!(!ptr::eq(&five, &other_five));

    let p: *const i32 = ptr::null();
    assert!(p.is_null());

    let pm: *mut i32 = ptr::null_mut();
    assert!(pm.is_null());

    let x = Box::new(12);

    unsafe {
        assert_eq!(12, ptr::read(&*x as *const i32));
    }

    assert_eq!(12, *x);

    let mut x = 0;
    let y = &mut x as *mut i32;
    let z = 12;

    unsafe {
        ptr::write(y, z);
        assert_eq!(12, ptr::read(y));
    }
}
