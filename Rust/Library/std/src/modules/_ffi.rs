pub fn ffi() {
    // cstring_struct();
}

fn cstring_struct() {
    use std::ffi::CString;
    use std::os::raw::c_char;

    extern {
        fn my_printer(s: *const c_char);
    }

    let c_to_print = CString::new("Hello, world!").unwrap();
    unsafe {
        my_printer(c_to_print.as_ptr());
    }

    let raw = b"foo".to_vec();
    unsafe {
        let c_string = CString::from_vec_unchecked(raw);
    }
}
