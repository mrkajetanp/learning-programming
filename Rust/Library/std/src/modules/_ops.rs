pub fn _ops() {
    math_ops();
    fn_ops();
}

fn math_ops() {
    use std::ops::{Add, Sub};

    #[derive(Debug)]
    struct Point {
        x: i32,
        y: i32,
    }

    impl Add for Point {
        type Output = Point;

        fn add(self, other: Point) -> Point {
            Point {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }
    }

    impl Sub for Point {
        type Output = Point;

        fn sub(self, other: Point) -> Point {
            Point {
                x: self.x - other.x,
                y: self.y - other.y,
            }
        }
    }

    println!("{:?}", Point {x: 1, y: 0} + Point {x: 2, y: 3});
    println!("{:?}", Point {x: 1, y: 0} - Point {x: 2, y: 3});
}

fn fn_ops() {
    fn call_with_one<F>(func: F) -> usize
        where F: Fn(usize) -> usize {

        func(1)
    }

    let double = |x| x * 2;
    assert_eq!(2, call_with_one(double));

    fn do_twice<F>(mut func: F)
        where F: FnMut() {

        func();
        func();
    }

    let mut x: usize = 1;
    {
        let add_two_to_x = || x += 2;
        do_twice(add_two_to_x);
    }
    assert_eq!(5, x);

    fn consume_with_relish<F>(func: F)
        where F: FnOnce() -> String {

        println!("Consumed: {}", func());
        println!("Delicious!");
    }

    let x = String::from("x");
    let consume_and_return_x = move || x;
    consume_with_relish(consume_and_return_x);
}
