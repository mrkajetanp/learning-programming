pub fn traits() {
    println!("***Traits***");

    basics();
    derive();
    operator_overloading();
    dropping();
    iterators();
    cloning();

    println!("");
}

fn basics() {
    struct Sheep {
        naked: bool,
        name: &'static str,
    }

    trait Animal {
        // static method signature
        // self is an implemented type
        fn new(name: &'static str) -> Self;

        // instance method signatures
        fn name(&self) -> &'static str;
        fn noise(&self) -> &'static str;

        // traits can provide default method definitions
        fn talk(&self) {
            println!("{} says {}", self.name(), self.noise());
        }
    }

    impl Sheep {
        fn is_naked(&self) -> bool {
            self.naked
        }

        fn shear(&mut self) {
            if self.is_naked() {
                // implementor methods can use its trait methods
                println!("{} is already naked", self.name());
            } else {
                println!("{} gets a haircut!", self.name());

                self.naked = true;
            }
        }
    }

    impl Animal for Sheep {
        fn new(name: &'static str) -> Sheep {
            Sheep {
                name: name,
                naked: false,
            }
        }

        fn name(&self) -> &'static str {
            self.name
        }

        fn noise(&self) -> &'static str {
            if self.is_naked() {
                "baaah?"
            } else {
                "baaaaaah!"
            }
        }

        // default trait methods can be overriden
        fn talk(&self) {
            println!("{} pauses briefly.. {}", self.name, self.noise());
        }
    }

    let mut dolly: Sheep = Animal::new("Dolly");

    dolly.talk();
    dolly.shear();
    dolly.talk();
}

fn derive() {
    // centimeters, a comparable tuple struct
    #[derive(PartialEq, PartialOrd)]
    struct Centimetrs(f64);

    // inches, a printable tuple struct
    #[derive(Debug)]
    struct Inches(i32);

    impl Inches {
        fn to_centimeters(&self) -> Centimetrs {
            let &Inches(inches) = self;
            Centimetrs(inches as f64 * 2.54)
        }
    }

    // seconds, a tuple struct without attributes
    struct Seconds(i32);

    let foot = Inches(12);

    println!("One foot equals: {:?}", foot);

    let meter = Centimetrs(100.0);

    let cmp =
        if foot.to_centimeters() < meter {
            "smaller"
        } else {
            "bigger"
        };

    println!("one foot is {} than one meter", cmp);
}

fn operator_overloading() {
    use std::ops;

    struct Foo;
    struct Bar;

    #[derive(Debug)]
    struct FooBar;

    #[derive(Debug)]
    struct BarFoo;

    // Foo + Bar = FooBar
    impl ops::Add<Bar> for Foo {
        type Output = FooBar;

        fn add(self, _rhs: Bar) -> FooBar {
            println!("> Foo.add(Bar) was called");

            FooBar
        }
    }

    // Bar + Foo = BarFoo
    impl ops::Add<Foo> for Bar {
        type Output = BarFoo;

        fn add(self, _rhs: Foo) -> BarFoo {
            println!("> Bar.add(Foo) was called");

            BarFoo
        }
    }

    println!("Foo + Bar = {:?}", Foo + Bar);
    println!("Bar + Foo = {:?}", Bar + Foo);
}

fn dropping() {
    struct Droppable {
        name: &'static str,
    }

    impl Drop for Droppable {
        fn drop(&mut self) {
            println!("> Dropping {}", self.name);
        }
    }

    let _a = Droppable { name: "a" };

    { // block A
        let _b = Droppable { name: "b" };

        { // block B
            let _c = Droppable { name: "c" };
            let _d = Droppable { name: "d" };

            println!("Exiting block B!");
        }
        println!("Just exited block B!");

        println!("Exiting block A!");
    }
    println!("Just exited block A!");

    drop(_a);

    println!("end of the function!");
}

fn iterators() {
    struct Fibonacci {
        curr: u32,
        next: u32,
    }

    impl Iterator for Fibonacci {
        type Item = u32;

        fn next(&mut self) -> Option<u32> {
            let new_next = self.curr + self.next;

            self.curr = self.next;
            self.next = new_next;

            Some(self.curr)
        }
    }

    fn fibonacci() -> Fibonacci {
        Fibonacci { curr: 1, next: 1 }
    }

    let mut sequence = 0..3;

    println!("Four consecutive 'next' calls on 0..3");
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());

    println!("iterate through 0..3 using 'for'");
    for i in 0..3 {
        println!("> {}", i);
    }

    // take(n) reduces an iterator to its first n terms
    println!("The first four terms of the Fibonacci sequence:");
    for i in fibonacci().take(4) {
        print!("{} ", i);
    }
    println!("");

    println!("The next four terms of the Fibonacci sequence:");
    for i in fibonacci().skip(4).take(4) {
        print!("{} ", i);
    }
    println!("");

    let array = [1_u32, 3, 3, 7];

    println!("Iterate the following array: {:?}", &array);
    for i in array.iter() {
        print!("{} ", i);
    }
    println!("");
}

fn cloning() {
    #[derive(Debug, Clone, Copy)]
    struct Nil;

    // a tuple struct with resources implementing the Clone trait
    #[derive(Clone, Debug)]
    struct Pair(Box<i32>, Box<i32>);

    let nil = Nil;
    let copied_nil = nil;

    println!("original nil: {:?}", nil);
    println!("copied nil: {:?}", copied_nil);

    let pair = Pair(Box::new(1), Box::new(2));
    println!("original: {:?}", pair);

    let moved_pair = pair;
    println!("moved pair: {:?}", moved_pair);

    let cloned_pair = moved_pair.clone();

    drop(moved_pair);

    // clone still can be used after dropping moved_pair
    println!("cloned pair: {:?}", cloned_pair);
}
