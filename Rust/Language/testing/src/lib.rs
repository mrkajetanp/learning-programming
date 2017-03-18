
// Documentation tests
// Module-level documentation
//! The 'testing' crate provides functions that add numbers to other numbers
//!
//! # Examples
//!
//! ```
//! assert_eq!(4, testing::add_two(2));
//! ```

// Function-level documentation
/// This function adds two to its argument.
///
/// # Examples
///
/// ```
/// use testing::add_two;
/// assert_eq!(4, add_two(2));
/// ```
pub fn add_two(a: i32) -> i32 {
    a + 2
}

// Test run concurrently by default
// to make them run without concurrency:
// cargo test -- --test-threads=1

// stop cargo from capturing standard output
// cargo test -- --nocapture

#[cfg(test)]
mod tests {
    use super::*;
    // test passes if it does not panic
    #[test]
    fn it_works_one() {
        // assert!(false);
    }

    // test passes if it panics
    #[test]
    #[should_panic]
    fn it_works_two() {
        assert_eq!("test", "not test");
    }

    // test passes if it panics with "assertion failed"
    #[test]
    #[should_panic(expected = "assertion failed")]
    fn it_works_three() {
        // panic!("why not?");
        assert_eq!("hello", "world");
    }

    // real test
    #[test]
    fn test_add_two() {
        assert_eq!(4, add_two(2));
    }

    #[test]
    #[ignore]
    fn expensive_test() {
        // assert!(false);
    }
}

