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

mod _collections;

pub fn modules_main() {
    // _any::_any();
    // _ascii::_ascii();
    // _borrow::_borrow();
    // _boxed::_boxed();
    // _cell::_cell();
    // _clone::_clone();
    // _cmp::_cmp();
    _collections::_collections();
}
