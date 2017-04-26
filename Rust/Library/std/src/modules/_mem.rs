pub fn _mem() {
    functions();
}

fn functions() {
    use std::mem;
    use std::ptr;

    // alignment used for struct fields
    assert_eq!(4, mem::align_of::<i32>());
    assert_eq!(4, mem::align_of_val(&5_i32));

    let v = vec![1, 2, 3];
    drop(v); // explicitly drop the vector

    let v = vec![1, 2, 3];
    let x = &v[0];
    drop(x);
    // v.push(4); // error - borrow still exists

    let mut v = vec![1, 2, 3];
    {
        let x = &v[0];

        drop(x);
    }
    v.push(4);

    unsafe {
        let mut uninit_vec: Vec<u32> = mem::uninitialized();

        let some_cond = false;

        if some_cond {
            // initialize the variable
            ptr::write(&mut uninit_vec, Vec::new());
        } else {
            // forget the value so its destructor doesn't run
            mem::forget(uninit_vec);
        }
    }

    let mut v: Vec<i32> = vec![1, 2];

    let old_v = mem::replace(&mut v, vec![3, 4, 5]);
    assert_eq!(2, old_v.len());
    assert_eq!(3, v.len());

    assert_eq!(4, mem::size_of::<i32>());
    assert_eq!(8, mem::size_of_val(&5.0_f64));

    let mut x = 5;
    let mut y = 42;

    mem::swap(&mut x, &mut y);

    assert_eq!(42, x);
    assert_eq!(5, y);

    // transmute
    // transmute_copy
    // zeroed
    // discriminant
}
