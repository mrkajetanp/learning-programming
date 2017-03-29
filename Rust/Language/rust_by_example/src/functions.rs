#![allow(dead_code)]
pub fn functions() {
    println!("***Functions***");

    basics();
    methods();
    closures();
    closures_std_examples();
    higher_order_functions();

    println!("");
}

fn is_divisible_by(lhs: u32, rhs: u32) -> bool {
    if rhs == 0 {
        return false;
    }

    lhs % rhs == 0
}

fn fizzbuzz(n: u32) {
    if is_divisible_by(n, 15) {
        println!("fizzbuzz");
    } else if is_divisible_by(n, 3) {
        println!("fizz");
    } else if is_divisible_by(n, 5) {
        println!("buzz");
    } else {
        println!("{}", n);
    }
}

fn fizzbuzz_to(n: u32) {
    for n in 1..n+1 {
        fizzbuzz(n);
    }
}

fn basics() {
    fizzbuzz_to(15);
}

struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn origin() -> Point {
        Point { x: 0.0, y: 0.0 }
    }

    fn new(x: f64, y: f64) -> Point {
        Point { x: x, y: y }
    }
}

struct Rectangle {
    p1: Point,
    p2: Point,
}

impl Rectangle {
    fn area(&self) -> f64 {
        let Point { x: x1, y: y1 } = self.p1;
        let Point { x: x2, y: y2 } = self.p2;

        ((x1-x2) * (y1-y2)).abs()
    }

    fn perimeter(&self) -> f64 {
        let Point { x: x1, y: y1 } = self.p1;
        let Point { x: x2, y: y2 } = self.p2;

        2.0 * ((x1-x2).abs() + (y1-y2).abs())
    }

    fn translate(&mut self, x: f64, y: f64) {
        self.p1.x += x;
        self.p2.x += x;

        self.p1.y += y;
        self.p2.y += y;
    }
}

struct Pair(Box<i32>, Box<i32>);

impl Pair {
    // method "consumes" self so it gets destroyed after going out of scope
    fn destroy(self) {
        let Pair(first, second) = self;

        println!("Destroying a pair ({}, {})", first, second);
    } // first and second go out of scope and get freed
}

fn methods() {
    let rectangle = Rectangle {
        p1: Point::origin(),
        p2: Point::new(3.0, 4.0),
    };

    println!("Rectangle perimeter: {}", rectangle.perimeter());
    println!("Rectangle area: {}", rectangle.area());

    let mut square = Rectangle {
        p1: Point::origin(),
        p2: Point::new(1.0, 1.0),
    };

    square.translate(1.0, 1.0);

    let pair = Pair(Box::new(1), Box::new(2));

    pair.destroy();
}

fn apply<F>(f: F) where F: FnOnce() {
    f();
}

fn apply_print<F>(f: F) where F: Fn() {
    f();
}

fn apply_to_3<F>(f: F) -> i32 where F: Fn(i32) -> i32 {
    f(3)
}

fn closures() {
    let closure_annotated = |i: i32| -> i32 { i + 1 };
    let closure_inferred = |i| i + 1;

    let i = 1;
    println!("closure_annotated: {}", closure_annotated(i));
    println!("closure inferred: {}", closure_inferred(i));

    // no arguments, returns 1
    let one = || 1;
    println!("closure returning one: {}", one());

    use std::mem;

    let color = "green";
    let print = || println!("color: {}", color);

    // call the closures using the borrow
    print();
    print();

    let mut count = 0;

    // closure has to be mut because it stores &mut count
    let mut inc = || {
        count += 1;
        println!("count: {}", count);
    };

    inc();
    inc();

    // a non-copy type
    let movable = Box::new(3);

    // mem::drop requires T so this must take by value
    // non-copy type must move so movable immediately moves into the closure
    let consume = || {
        println!("movable: {:?}", movable);
        mem::drop(movable);
    };

    // it consumes the variable so it can only be called once
    consume();

    let greeting = "hello";
    // non-copy type, data is owned
    let mut farewell = "goodbye".to_owned();

    // captures greeting by ref and farewell by value
    let diary = || {
        // greeting is by reference, requires Fn
        println!("I said {}", greeting);

        // mutation forces farewell to be captured by mutable reference
        // now requires FnMut
        farewell.push_str("!!!");
        println!("Then I screamed {}", farewell);
        println!("Now I can sleep..");

        // manually dropping forces 'farewell' to be captured by value
        // now requires FnOnce
        mem::drop(farewell);
    };

    apply(diary);
    let double = |x| 2 * x;
    println!("3 doubled: {}", apply_to_3(double));

    // type anonymity

    let x = 7;

    // capture x into an anonymous type and implement Fn for it
    let print = || println!("{}", x);

    apply_print(print);

    fn call_me<F: Fn()>(f: F) {
        f();
    }

    // function satisfying the 'Fn' bound
    fn function() {
        println!("I'm a function!");
    }

    let closure = || println!("I'm a closure!");
    call_me(closure);
    call_me(function);

    // as output parameters

    fn create_fn() -> Box<Fn()> {
        let text = "Fn".to_owned();

        // move singnals that all captures occur by value
        Box::new(move || println!("This is a: {}", text))
    }

    fn create_fnmut() -> Box<FnMut()> {
        let text = "FnMut".to_owned();
        Box::new(move || println!("This is a: {}", text))
    }

    let fn_plain = create_fn();
    let mut fn_mut = create_fnmut();

    fn_plain();
    fn_mut();
}

fn closures_std_examples() {
    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];

    println!("2 in vec1: {}", vec1.iter().any(|&x| x == 2));
    println!("2 in vec2: {}", vec2.into_iter().any(|x| x == 2));

    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];
    let mut iter = vec1.iter();
    let mut into_iter = vec2.into_iter();

    println!("find 2 in vec1: {:?}", iter.find(|&&x| x == 2));
    println!("find 2 in vec2: {:?}", into_iter.find(|&x| x == 2));
}

fn higher_order_functions() {
    fn is_odd(n: u32) -> bool {
        n % 2 == 1
    }

    let upper = 1000;

    let sum_of_squared_odd_numbers: u32 =
        (0..).map(|n| n * n) // all natural numbers squared
        .take_while(|&n| n < upper) // below upper limit
        .filter(|&n| is_odd(n)) // that are odd
        .fold(0, |sum, i| sum + i); // sum them

    println!("functional style: {}", sum_of_squared_odd_numbers);
}
