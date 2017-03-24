extern crate libc;

mod snappy;
mod advanced_ffi;

fn main() {
    snappy::snappy_main();
    advanced_ffi::adv_ffi();
}
