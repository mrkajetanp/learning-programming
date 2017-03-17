#![allow(dead_code, unused_unsafe)]

pub fn unsafes() {

    // unsafe block
    unsafe {
        // unsafe code here
    }

}

unsafe trait Scary { }

unsafe impl Scary for i32 { }

unsafe fn dangerous_function() {
    // scary stuff here...
}
