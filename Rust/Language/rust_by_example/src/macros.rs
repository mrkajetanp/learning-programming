pub fn macros() {
    println!("***Macros***");

    basics();
    designators();
    overload();
    repeat();
    dry();

    println!("");
}

fn basics() {
    macro_rules! say_hello {
        () => (
            // macro will expand to this
            println!("> say_hello macro called!");
            println!("Hello!");
        )
    }

    say_hello!();
}

fn designators() {
    macro_rules! create_function {
        // this macro takes an argument of designator 'ident'
        // and creates a function named $func_name
        // the ident designator is used for variable/function names
        ($func_name:ident) => (
            fn $func_name() {
                println!("You called {:?}()", stringify!($func_name));
            }
        )
    }

    create_function!(foo);
    create_function!(bar);

    macro_rules! print_result {
        // takes an expression of type 'expr' and prints
        // it as a string along with its results
        // the expr designator is used for expressions
        ($expression:expr) => (
            println!("{:?} = {:?}", stringify!($expression), $expression)
        )
    }

    foo();
    bar();

    print_result!(1_u32 + 1);

    // blocks are expressions too
    print_result!({
        let x = 1_u32;
        x * x + 2 * x - 1
    });
}

fn overload() {
    macro_rules! test {
        ($left:expr; and $right:expr) => (
            println!("{:?} and {:?} is {:?}",
                     stringify!($left),
                     stringify!($right),
                     $left && $right)
        );
        // each arm must end with semicolon
        ($left:expr; or $right:expr) => (
            println!("{:?} or {:?} is {:?}",
                     stringify!($left),
                     stringify!($right),
                     $left || $right)
        );
    }

    test!(1_i32 + 1 == 2_i32; and 2_i32 * 2 == 4_i32);
    test!(true; or false);
}

use std;

fn repeat() {
    macro_rules! find_min {
        // base case
        ($x:expr) => ($x);
        // $x is followed by at least one $y
        ($x:expr, $($y:expr),+) => (
            // call find_min on the tail $y
            std::cmp::min($x, find_min!($($y),+))
        )
    }

    println!("{}", find_min!(1_u32));
    println!("{}", find_min!(1_u32 + 2, 2_u32));
    println!("{}", find_min!(5_u32, 2_u32 * 3, 4_u32));
}

fn dry() {
    use std::ops::{Add, Mul, Sub};

    macro_rules! assert_equal_len {
        // tt - token tree (for operators and tokens)
        ($a:ident, $b: ident, $func:ident, $op:tt) => (
            assert!($a.len() == $b.len(),
                    "{:?}: dimension mismatch: {:?} {:?} {:?}",
                    stringify!($func),
                    ($a.len(),),
                    stringify!($op),
                    ($b.len(),));
        )
    }

    macro_rules! op {
        ($func:ident, $bound:ident, $op:tt, $method:ident) => (
            fn $func<T: $bound<T, Output=T> + Copy>(xs: &mut Vec<T>, ys: &Vec<T>) {
                assert_equal_len!(xs, ys, $func, $op);

                for (x, y) in xs.iter_mut().zip(ys.iter()) {
                    *x = $bound::$method(*x, *y);
                    // *x = x.$method(*y);
                }
            }
        );
    }

    op!(add_assign, Add, +=, add);
    op!(mul_assign, Mul, *=, mul);
    op!(sum_assign, Sub, -=, sub);
}
