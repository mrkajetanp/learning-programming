#![allow(dead_code, unused_unsafe, private_no_mangle_fns)]

use libc::c_int;
use libc::c_char;
use std::ptr;
use std::ffi::CString;

pub fn adv_ffi() {
    println!("***Advanced FFI***");

    one();

    println!("");
}

// unsafe function
unsafe fn explosive(ptr: *const i32) -> i32 {
    *ptr
}

// accessing foreign globals
#[link(name = "readline")]
extern {
    static rl_readline_version: c_int;
    static mut rl_prompt: *const c_char;
}

// foreign variadic functions
extern {
    fn foo(x: i32, ...);
}

// this function can be called from C code
#[no_mangle]
pub extern fn hello_rust() -> *const u8 {
    "Hello, world!\0".as_ptr()
}

fn one() {
    println!("You have readline version {} installed.", unsafe { rl_readline_version as i32 });

    let prompt = CString::new("[my-awesome-shell] $").unwrap();
    unsafe {
        rl_prompt = prompt.as_ptr();

        println!("{:?}", rl_prompt);

        rl_prompt = ptr::null();
    }

    unsafe {
        // foo(10, 20, 30, 40, 50);
    }
}
