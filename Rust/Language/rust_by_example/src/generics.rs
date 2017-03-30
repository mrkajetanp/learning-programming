#![allow(dead_code, unused_variables)]
pub fn generics() {
    println!("***Generics***");

    basics();
    functions();
    implementation();
    traits();
    bounds();
    multiple_bounds();
    where_clauses();
    associated_items();
    associated_types();
    phantom_type_params();
    testcase_unit_conversion();

    println!("");
}

// a concrete type 'A'
struct A;

// a concrete struct with a concrete A
struct Single(A);

// SingleGen is a generic type
struct SingleGen<T>(T);

fn basics() {
    let _s = Single(A);

    let _char: SingleGen<char> = SingleGen('a');

    let _t = SingleGen(A); // uses 'A' defined at the top
    let _i32 = SingleGen(6); // uses 'i32'
    let _char = SingleGen('a'); // uses 'char'
}

fn functions() {
    struct A; // concrete type
    struct S(A); // concrete type
    struct SGen<T>(T); // generic type

    // functions take ownership of the argument
    // and free them when they go out of scope

    // not generic functions
    fn reg_fn(_s: S) {}
    fn gen_spec_t(_s: SGen<A>) {}
    fn gen_spec_i32(_s: SGen<i32>) {}

    // generic function
    fn generic<T>(_s: SGen<T>) {}

    reg_fn(S(A));
    gen_spec_t(SGen(A));
    gen_spec_i32(SGen(6));

    // explicitly specified char type parameter to generic()
    generic::<char>(SGen('a'));

    // implicitly specified char type parameter to generic()
    generic(SGen('c'));
}

fn implementation() {
    struct Val {
        val: f64
    }

    struct GenVal<T> {
        gen_val: T
    }

    impl Val {
        fn value(&self) -> &f64 {
            &self.val
        }
    }

    impl<T> GenVal<T> {
        fn value(&self) -> &T {
            &self.gen_val
        }
    }

    let x = Val { val: 3.2 };
    let y = GenVal { gen_val: 3_i32 };
    let z = GenVal { gen_val: 3.8 };

    println!("{}, {}, {}", x.value(), y.value(), z.value());
}

fn traits() {
    struct Empty;
    struct Null;

    trait DoubleDrop<T> {
        fn double_drop(self, _: T);
    }

    impl<T, U> DoubleDrop<T> for U {
        // deallocates both passed arguments
        fn double_drop(self, _: T) {}
    }

    let empty = Empty;
    let null = Null;

    empty.double_drop(null);
}

fn bounds() {
    use std::fmt::Debug;

    trait HasArea {
        fn area(&self) -> f64;
    }

    impl HasArea for Rectangle {
        fn area(&self) -> f64 {
            self.length * self.height
        }
    }

    #[derive(Debug)]
    struct Rectangle {
        length: f64,
        height: f64,
    }

    struct Triangle {
        length: f64,
        height: f64,
    }

    fn print_debug<T: Debug>(t: &T) {
        println!("{:?}", t);
    }

    fn area<T: HasArea>(t: &T) -> f64 {
        t.area()
    }

    let rectangle = Rectangle { length: 3.0, height: 4.0 };
    let _triangle = Triangle { length: 3.0, height: 4.0 };

    print_debug(&rectangle);
    println!("Area: {}", area(&rectangle));

    // empty bounds

    struct Cardinal;
    struct BlueJay;
    struct Turkey;

    trait Red {}
    trait Blue {}

    impl Red for Cardinal {}
    impl Blue for BlueJay {}

    fn red<T: Red>(_: &T) -> &'static str {
        "red"
    }

    fn blue<T: Blue>(_: &T) -> &'static str {
        "blue"
    }

    let cardinal = Cardinal;
    let blue_jay = BlueJay;
    let _turkey = Turkey;

    println!("A cardinal is {}", red(&cardinal));
    println!("A blue jay is {}", blue(&blue_jay));

    // can't use it, sorry
    // println!("A turkey is {}", red(&turkey));
}

fn multiple_bounds() {
    use std::fmt::{Debug, Display};

    fn compare_prints<T: Debug + Display>(t: &T) {
        println!("Debug: {:?}", t);
        println!("Display: {}", t);
    }

    fn compare_types<T: Debug, U: Debug>(t: &T, u: &U) {
        println!("t: {:?}", t);
        println!("u: {:?}", u);
    }

    let string = "words";
    let array = [1, 2, 3];
    let vec = vec![1, 2, 3];

    compare_prints(&string);
    compare_types(&array, &vec);
}

fn where_clauses() {
    use std::fmt::Debug;

    trait PrintInOption {
        fn print_in_option(self);
    }

    impl<T> PrintInOption for T where
        Option<T>: Debug {

        fn print_in_option(self) {
            println!("{:?}", Some(self));
        }
    }

    let vec = vec![1, 2, 3];

    vec.print_in_option();
}

fn associated_items() {
    struct Container(i32, i32);

    trait Contains<A, B> {
        fn contains(&self, &A, &B) -> bool;
        fn first(&self) -> i32;
        fn last(&self) -> i32;
    }

    impl Contains<i32, i32> for Container {
        // true if the numbers stored are equal
        fn contains(&self, number_1: &i32, number_2: &i32) -> bool {
            (&self.0 == number_1) && (&self.1 == number_2)
        }

        fn first(&self) -> i32 { self.0 }
        fn last(&self) -> i32 { self.1 }
    }

    fn difference<A, B, C>(container: &C) -> i32 where
        C: Contains<A, B> {
        container.last() - container.first()
    }

    let number_1 = 3;
    let number_2 = 10;

    let container = Container(number_1, number_2);

    println!("Does container contain {} and {}: {}",
             &number_1, &number_2, container.contains(&number_1, &number_2));

    println!("First number: {}", container.first());
    println!("Last number: {}", container.last());

    println!("The difference is: {}", difference(&container));
}

fn associated_types() {
    struct Container(i32, i32);

    trait Contains {
        type A;
        type B;

        fn contains(&self, &Self::A, &Self::B) -> bool;
        fn first(&self) -> i32;
        fn last(&self) -> i32;
    }

    impl Contains for Container {
        type A = i32;
        type B = i32;

        // we might as well use &Self::A here
        fn contains(&self, number_1: &i32, number_2: &i32) -> bool {
            (&self.0 == number_1) && (&self.1 == number_2)
        }

        fn first(&self) -> i32 {
            self.0
        }

        fn last(&self) -> i32 {
            self.1
        }
    }

    fn difference<C: Contains>(container: &C) -> i32 {
        container.last() - container.first()
    }

    let number_1 = 3;
    let number_2 = 10;

    let container = Container(number_1, number_2);

    println!("Does container contain {} and {}: {}",
             &number_1, &number_2, container.contains(&number_1, &number_2));

    println!("First number: {}", container.first());
    println!("Last number: {}", container.last());

    println!("The difference is: {}", difference(&container));
}

fn phantom_type_params() {
    use std::marker::PhantomData;

    #[derive(PartialEq)]
    struct PhantomTuple<A, B>(A, PhantomData<B>);

    #[derive(PartialEq)]
    struct PhantomStruct<A, B> {
        first: A,
        phantom: PhantomData<B>
    }

    // storage is not allocated for B, so it can't be used in computation
    // it exists only at compile time

    let _tuple1: PhantomTuple<char, f32> = PhantomTuple('Q', PhantomData);
    let _tuple2: PhantomTuple<char, f64> = PhantomTuple('Q', PhantomData);

    let _struct1: PhantomStruct<char, f32> = PhantomStruct {
        first: 'Q',
        phantom: PhantomData,
    };

    let _struct2: PhantomStruct<char, f64> = PhantomStruct {
        first: 'Q',
        phantom: PhantomData,
    };

    // nothing here can be compared because of type mismatch
}

fn testcase_unit_conversion() {
    use std::ops::Add;
    use std::marker::PhantomData;

    #[derive(Debug, Clone, Copy)]
    enum Inch { }
    #[derive(Debug, Clone, Copy)]
    enum Mm { }

    #[derive(Debug, Clone, Copy)]
    struct Length<Unit>(f64, PhantomData<Unit>);

    impl<Unit> Add for Length<Unit> {
        type Output = Length<Unit>;

        fn add(self, rhs: Length<Unit>) -> Length<Unit> {
            Length(self.0 + rhs.0, PhantomData)
        }
    }

    let one_foot: Length<Inch> = Length(12.0, PhantomData);

    let one_meter: Length<Mm> = Length(1000.0, PhantomData);

    let two_feet = one_foot + one_foot;
    let two_meters = one_meter + one_meter;

    println!("one foot + one foot = {:?} in", two_feet.0);
    println!("one meter + one meter = {:?} mm", two_meters.0);
}
