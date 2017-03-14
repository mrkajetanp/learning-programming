pub fn attributes() {
    println!("***Attributes***");

    one();

    println!("");
}

// #[test] - applies to the next item
// #![test] - applies to the item enclosing it

// check is now a test function
#[test]
fn check() {
    assert_eq!(2, 1+1);
}

#[allow(dead_code)]
#[inline(always)]
fn super_fast_fn() { }

#[cfg(target_os = "linux")]
fn one() {
}
