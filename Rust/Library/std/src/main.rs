#![allow(unused_variables, dead_code, unused_must_use)]

// #![feature(get_type_id)]
#![feature(iter_rfind)]

extern crate libc;

#[cfg(if_types)]
mod primitive_types;

mod modules;

fn main() {
    // primitive_types::primitive_types_main();
    modules::modules_main();
}
