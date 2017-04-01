pub fn scoping() {
    println!("***Scoping***");

    raii();
    ownership_and_moves();
    borrowing();
    lifetimes();

    println!("");
}

fn raii() {
    fn create_box() {
        // allocate an i32 on the heap
        let _box1 = Box::new(3_i32);

        // box gets destroyed here, memory gets freed
    }

    let _box2 = Box::new(5_i32);

    // a nested scope

    {
        let _box3 = Box::new(4_i32);

        // _box3 is destroyed here
    }

    // creating lots of boxes just for fun
    // they're not bound to anything so they get immediately destroyed
    for _ in 0_u32..1_000 {
        create_box();
    }

    // _box2 destroyed here
}

fn ownership_and_moves() {
    fn destroy_box(c: Box<i32>) {
        println!("Destroying a box that contains {}", c);

        // c is destroyed and memory freed
    }

    // a stack allocated integer
    let x = 5_u32;

    // x gets copied because u32 is a Copy type & it's on the stack
    let y = x;

    // both can be accessed
    println!("x is {}, and y is {}", x, y);

    // a is a ptr to heap allocated i32
    let a = Box::new(5_i32);

    println!("a contains: {}", a);

    // moving a into b
    let b = a;

    println!("b contains: {}", b);

    // a does not own it anymore, it can't be accessed
    // println!("a contains: {}", a);

    destroy_box(b);

    // Heap memory has been freed at this point, can't access b
    // println!("b now contains: {}", b);

    // mutability

    let immutable_box = Box::new(5_u32);

    println!("immutable box contains: {}", immutable_box);

    // mutability error
    // *immutable_box = 4;

    // move it into the mutable binding, changes mutability
    let mut mutable_box = immutable_box;

    println!("mutable_box contains: {}", mutable_box);

    *mutable_box = 4;

    println!("mutable_box now contains: {}", mutable_box);
}

fn borrowing() {
    // function takes ownership of a box and destroyes it
    fn eat_box_i32(boxed_i32: Box<i32>) {
        println!("Eating a box that contains: {}", boxed_i32);
    }

    // this function borrows an i32
    fn borrow_i32(borrowed_i32: &i32) {
        println!("This int is: {}", borrowed_i32);
    }

    // create a boxed i32 and stacked i32
    let boxed_i32 = Box::new(5_i32);
    let stacked_i32 = 6_i32;

    // borrow the contents of the box
    // does not take ownership -> contents can be borrowed again
    borrow_i32(&boxed_i32);
    borrow_i32(&stacked_i32);

    {
        // take a reference to the data inside the box
        let _ref_to_i32: &i32 = &boxed_i32;

        // can't destroy boxed_i32 while the inner value is borrowed
        // eat_box_i32(boxed_i32);
    } // ref_to_i32 goes out of scope - no longer borrowed

    // no more borrows
    // now it can give up its ownership and be destroyed
    eat_box_i32(boxed_i32);

    /* mutability */

    #[derive(Clone, Copy)]
    struct Book {
        author: &'static str,
        title: &'static str,
        year: u32,
    }

    fn borrow_book(book: &Book) {
        println!("I immmutably borrowed {} - {} edition", book.title, book.year);
    }

    fn new_edition(book: &mut Book) {
        book.year = 2017;
        println!("I mutably borrowed {} - {} edition", book.title, book.year);
    }

    let immutabook = Book {
        author: "George R. R. Martin",
        title: "A Game of Thrones",
        year: 1996,
    };

    // mutable copy of immutabook
    let mut mutabook = immutabook;

    borrow_book(&immutabook);

    borrow_book(&mutabook);

    new_edition(&mut mutabook);

    // can't borrow immutable object as mutable
    // new_edition(&mut immutabook);

    /* freezing */

    let mut _mutable_int = 7_i32;

    {
        let _large_int = &_mutable_int;

        // _mutable_int is frozen because it was borrowed!
        // _mutable_int = 50;
    }

    // it's okay here, not borrowed anymore
    _mutable_int = 3;

    struct Point {
        x: i32,
        y: i32,
        z: i32,
    }

    let mut point = Point {
        x: 0,
        y: 0,
        z: 0,
    };

    {
        let borrowed_point = &point;
        let another_borrow = &point;

        // data can be accessed by references and the original owner
        println!("Point is: ({},{},{})", borrowed_point.x, another_borrow.y, point.z);

        // can't borrow mutably - it's already borrowed as immutable
        // let mutable_borrow = &mut point;
    }

    {
        let mutable_borrow = &mut point;

        mutable_borrow.x = 5;
        mutable_borrow.y = 2;
        mutable_borrow.z = 1;

        // can't borrow as immutable - currently borrowed as mutable
        // let y = &point.y;

        // mutable references can be passed as immutable to println

        println!("Point is: ({},{},{})", mutable_borrow.x, mutable_borrow.y, mutable_borrow.z);
    }

    // immutable references to point are allowed again
    let borrowed_point = &point;
    println!("Point is: ({},{},{})", borrowed_point.x, borrowed_point.y, borrowed_point.z);

    /* the ref pattern */
    {
        #[derive(Clone, Copy)]
        struct Point {
            x: i32,
            y: i32,
        }

        let c = 'Q';

        // both have the same effect
        let ref ref_c1 = c;
        let ref_c2 = &c;

        println!("ref_c1 equals ref_c2: {}", *ref_c1 == *ref_c2);

        let point = Point { x: 0, y: 0 };

        let _copy_of_x = {
            // ref_to_x points to the x field of a point
            let Point { x: ref ref_to_x, y: _ } = point;

            // return a copy of the 'x' field
            *ref_to_x
        };

        // a mutable copy of 'point'
        let mut mutable_point = point;

        {
            let Point { x: _, y: ref mut mut_ref_to_y } = mutable_point;

            *mut_ref_to_y = 1;
        }

        println!("point is: ({},{})", point.x, point.y);
        println!("mutable point is: ({},{})", mutable_point.x, mutable_point.y);

        // a mutable tuple that includes a pointer
        let mut mutable_tuple = (Box::new(5_u32), 3_u32);

        {
            let (_, ref mut last) = mutable_tuple;
            *last = 2_u32;
        }

        println!("tuple is: {:?}", mutable_tuple);
    }
}

fn lifetimes() {
    /* explicit annotation */
    {
        // fn takes two references to i32 which have different lifetimes
        // 'a and 'b. These two lifetimes must both be at least as long
        // as the function print_refs
        fn print_refs<'a, 'b>(x: &'a i32, y: &'b i32) {
            println!("x is {} and y is {}", x, y);
        }

        // a function which takes no arguments
        // but has a lifetime parameter a
        fn failed_borrow<'a>() {
            let _x = 12;

            // _x does not live long enough
            // let y: &'a i32 = &_x;
            // attempting to use the lifetime 'a as an explicit type annotation
            // inside the function will fail because the lifetime &_x is shorter
            // than that of y
        }

        let (four, nine) = (4, 9);
        // passes borrows into a function
        print_refs(&four, &nine);
        // any input which is borrowed must outlive the borrower
        // the lifetime of four and nine must be longer
        // than that of print_refs

        failed_borrow();
        // it contains no references to force 'a to be
        // longer than the lifetime of the function but 'a is longer
        // because the lifetime is not constrained, it defaults to static
    }

    /* functions */
    {
        // input with lifetime 'a must live at least as long as the function
        fn print_one<'a>(x: &'a i32) {
            println!("print_one: x is {}", x);
        }

        // mutable references are possible with lifetimes as well
        fn add_one<'a>(x: &'a mut i32) {
            *x += 1;
        }

        // multiple elements with different lifetimes
        // in this case it would be fine for both to have the same lifetime 'a
        fn print_multi<'a, 'b>(x: &'a i32, y: &'b i32) {
            println!("print_multi: x is {}, y is {}", x, y);
        }

        // returning references that have been passed in is acceptable
        // the correct lifetime must be returned
        fn pass_x<'a, 'b>(x: &'a i32, _: &'b i32) -> &'a i32 {
            x
        }

        let (x, y) = (7, 9);
        print_one(&x);
        print_multi(&x, &y);

        let z = pass_x(&x, &y);
        print_one(z);

        let mut t = 3;
        add_one(&mut t);
        print_one(&t);
    }

    /* methods */
    {
        struct Owner(i32);

        impl Owner {
            fn add_one<'a>(&'a mut self) {
                self.0 += 1
            }

            fn print<'a>(&'a self) {
                println!("print: {}", self.0);
            }
        }

        let mut owner = Owner(18);
        owner.add_one();
        owner.print();
    }

    /* structs */
    {
        // a reference to i32 must outlive the struct
        #[derive(Debug)]
        struct Borrowed<'a>(&'a i32);

        // both references must outlive the structure
        #[derive(Debug)]
        struct NamedBorrowed<'a> {
            x: &'a i32,
            y: &'a i32,
        }

        // an enum which is either an i32 or a reference to one
        #[derive(Debug)]
        enum Either<'a> {
            Num(i32),
            Ref(&'a i32),
        }

        let x = 18;
        let y = 15;

        let single = Borrowed(&x);
        let double = NamedBorrowed {
            x: &x,
            y: &y,
        };
        let reference = Either::Ref(&x);
        let number = Either::Num(y);

        println!("x is borrowed in {:?}", single);
        println!("x and y are borrowed in {:?}", double);
        println!("x is borrowed in {:?}", reference);
        println!("y is *not* borrowed in {:?}", number);
    }

    /* bounds */
    {
        use std::fmt::Debug;

        #[derive(Debug)]
        struct Ref<'a, T: 'a>(&'a T);
        // Ref contains a reference to generic type T that has an unknown lifetime 'a
        // T is bounded such that any references in T must outlive 'a
        // the lifetime of Ref may not exceed 'a

        fn print<T>(t: T) where T: Debug {
            println!("print: t is {:?}", t);
        }

        // a reference to T is taken where T implements Debug
        // all references in T outlive 'a
        // 'a must outlive the function
        fn print_ref<'a, T>(t: &'a T) where T: Debug + 'a {
            println!("print_ref: t is {:?}", t);
        }

        let x = 7;
        let ref_x = Ref(&x);

        print_ref(&ref_x);
        print(ref_x);
    }

    /* coercion */
    {
        // Rust infers a lifetime that is as short as possible
        // two references are then coerced to that lifetime
        fn multiply<'a>(first: &'a i32, second: &'a i32) -> i32 {
            first * second
        }

        // <'a: 'b, 'b> - lifetime 'a is at least as long as 'b
        // we take in an &'a i32 and return a &'b i32 as a result of coercion
        fn choose_first<'a: 'b, 'b>(first: &'a i32, _: &'b i32) -> &'b i32 {
            first
        }

        let first = 2; // longer lifetime

        {
            let second = 3; // shorter lifetime
            println!("the product is {}", multiply(&first, &second));

            println!("{} is the first", choose_first(&first, &second));
        }
    }

    /* static */
    {
        // a constant with a 'static lifetime
        static NUM: i32 = 18;

        // returns a reference to NUM where its static
        // lifetime is coerced to that of the input argument
        fn coerce_static<'a>(_: &'a i32) -> &'a i32 {
            &NUM
        }

        {
            let static_string = "I'm in read-only memory";
            println!("static_string: {}", static_string);

            // when the reference goes out of scope, str can no longer be used
            // but the data remains in the binary
        }

        {
            let lifetime_num = 9;

            // coerce NUM to the lifetime of lifetime_num
            let coerced_static = coerce_static(&lifetime_num);

            println!("coerced static: {}", coerced_static);
        }

        println!("NUM: {} stays accessible!", NUM);
    }

    /* elision */
    {
        fn elided_input(x: &i32) {
            println!("elided_input: {}", x);
        }

        fn annotated_input<'a>(x: &'a i32) {
            println!("annotated_input: {}", x);
        }

        fn elided_pass(x: &i32) -> &i32 {
            x
        }

        fn annotated_pass<'a>(x: &'a i32) -> &'a i32 {
            x
        }

        let x = 3;

        elided_input(&x);
        annotated_input(&x);

        println!("elided_pass: {}", elided_pass(&x));
        println!("annotated pass: {}", annotated_pass(&x));
    }
}
