
// in lib.rs
//! Tih sis documentation for the `foo` crate.
//!
//! The foo crate is meant to be used for bar.

/// Constructs a new `Rc<T>`
///
// important sections
/// # Examples
///
/// # Panics
///
/// # Errors
///
/// # Safety
///
/// ```
/// use std::rc::Rc;
///
/// let five = Rc::new(5);
/// ```
pub fn new(value: T) -> Rc<T> {
    // implementation goes here
}

#![warn(missing_docs)]

#![deny(missing_docs)]

// #[allow(missing_docs)]

// this struct is hidden from documentation
#[doc(hidden)]
struct Hidden;

/// The `Option` type.
enum Option<T> {
    /// No value
    None,
    /// Some value `T`
    Some(T),
}

mod foo {
    //! This is documentation for the `foo` module.
    //!
    //! # Examples

    // ...
}


