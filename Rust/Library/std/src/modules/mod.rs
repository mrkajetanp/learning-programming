#[cfg(off)]
mod _any;
#[cfg(off)]
mod _ascii;
#[cfg(off)]
mod _borrow;
#[cfg(off)]
mod _boxed;
#[cfg(off)]
mod _cell;
#[cfg(off)]
mod _clone;
#[cfg(off)]
mod _cmp;
#[cfg(off)]
mod _convert;
#[cfg(off)]
mod _default;
#[cfg(off)]
mod _collections;
#[cfg(off)]
mod _env;
#[cfg(off)]
mod _error;
// same with f64 module
#[cfg(off)]
mod _f32;
#[cfg(off)]
mod _ffi;
#[cfg(off)]
mod _fmt;
#[cfg(off)]
mod _hash;
#[cfg(off)]
mod _io;
#[cfg(off)]
mod _iter;
#[cfg(off)]
mod _marker;
#[cfg(off)]
mod _mem;
#[cfg(off)]
mod _num;
#[cfg(off)]
mod _ops;
#[cfg(off)]
mod _option;
#[cfg(off)]
mod _process;
#[cfg(off)]
mod _ptr;
#[cfg(off)]
mod _rc;
#[cfg(off)]
mod _result;
#[cfg(off)]
mod _slice;
mod _string;

pub fn modules_main() {
    // _any::_any();
    // _ascii::_ascii();
    // _borrow::_borrow();
    // _boxed::_boxed();
    // _cell::_cell();
    // _clone::_clone();
    // _cmp::_cmp();
    // _collections::_collections();
    // _convert::convert();
    // _default::default();
    // _env::env();
    // _error::error();
    // _f32::_f32();
    // _ffi::ffi();
    // _fmt::fmt();
    // _hash::_hash();
    // _io::_io();
    // _iter::_iter();
    // _marker::_marker();
    // _mem::_mem();
    // _num::_num();
    // _ops::_ops();
    // _option::_option();
    // _process::process();
    // _ptr::ptr();
    // _rc::rc();
    // _result::result();
    // _slice::slice();
    _string::string();
}
