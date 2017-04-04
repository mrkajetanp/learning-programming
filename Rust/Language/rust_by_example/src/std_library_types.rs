pub fn std_library_types() {
    println!("***STD Library Types***");

    box_stack_heap();
    vectors();
    strings();
    options();

    println!("");
}

fn box_stack_heap() {
    use std::mem;

    #[derive(Clone, Copy)]
    struct Point {
        x: f64,
        y: f64,
    }

    struct Rectangle {
        p1: Point,
        p2: Point,
    }

    fn origin() -> Point {
        Point { x: 0.0, y: 0.0 }
    }

    fn boxed_origin() -> Box<Point> {
        // allocate point in the heap and return a pointer to it
        Box::new(Point { x: 0.0, y: 0.0 })
    }


    // stack allocated variables
    let point: Point = origin();
    let rectangle: Rectangle = Rectangle {
        p1: origin(),
        p2: Point { x: 3.0, y: 4.0 },
    };

    // heap allocated rectangle
    let boxed_rectangle: Box<Rectangle> = Box::new(Rectangle {
        p1: origin(),
        p2: origin(),
    });

    // the output of functions can be boxed
    let boxed_point: Box<Point> = Box::new(origin());

    // double indirection
    let box_in_a_box: Box<Box<Point>> = Box::new(boxed_origin());

    println!("Point occupies {} bytes in the stack",
             mem::size_of_val(&point));

    println!("Rectangle occupies {} bytes in the stack",
             mem::size_of_val(&rectangle));

    // box size = pointer size
    println!("Boxed point occupies {} bytes in the stack",
             mem::size_of_val(&boxed_point));

    println!("Boxed rectangle occupies {} bytes in the stack",
             mem::size_of_val(&boxed_rectangle));

    println!("Boxed box occupies {} bytes in the stack",
             mem::size_of_val(&box_in_a_box));

    // copy the data contained in boxed_point into unboxed_point
    let unboxed_point: Point = *boxed_point;
    println!("Unboxed point occupies {} bytes in the stack",
             mem::size_of_val(&unboxed_point));
}

fn vectors() {
    // iterators can be collected into vectors
    let collected_iterator: Vec<i32> = (0..10).collect();
    println!("Collected (0..10) into: {:?}", collected_iterator);

    // the vec! macro can be used to initalize a vector
    let mut xs = vec![1_i32, 2, 3];
    println!("Initial vector: {:?}", xs);

    // insert a new element at the end
    xs.push(4);
    println!("Vector: {:?}", xs);

    // immutable vectors can't grow
    println!("Vector size: {}", xs.len());

    // inexing is done using the square brackets (indexing starts at 0)
    println!("Second element: {}", xs[1]);

    // pop removes the last element and returns it
    println!("Pop last element: {:?}", xs.pop());

    // Out of bounds indexing yields a panic
    // println!("Fourth element: {:?}", xs[3]);
}

fn strings() {
    // a reference to a string allocated in read only memory
    let pangram: &'static str = "the quick brown fox jumps over the lazy dog";
    println!("Pangram: {}", pangram);

    // iterate over words in reverse, no new string is allocated
    for word in pangram.split_whitespace().rev() {
        print!("{} ", word);
    }
    println!("");

    // copy chars into a vector, sort and remove duplicates
    let mut chars: Vec<char> = pangram.chars().collect();
    chars.sort();
    chars.dedup();
    println!("Chars: {:?}", chars);

    // create an empty and growable 'String'
    let mut string = String::new();
    for c in chars {
        // insert a char at the end of a string
        string.push(c);
        // insert a string at the end of a string
        string.push_str(", ");
    }

    println!("string: {}", string);

    // the trimmed string is a slice to the original string
    // so no allocation is performed
    let chars_to_trim: &[char] = &[' ', ','];
    let trimmed_str: &str = string.trim_matches(chars_to_trim);
    println!("used characters: {}", trimmed_str);

    // heap allocate a string
    let alice = String::from("I like dogs");
    // allocate a new memory and store the modified string there
    let bob: String = alice.replace("dog", "cat");

    println!("Alice says: {}", alice);
    println!("Bob says: {}", bob);
}

fn options() {
    // an int division that does not panic
    fn checked_division(divident: i32, divisor: i32) -> Option<i32> {
        if divisor == 0 {
            None
        } else {
            Some(divident / divisor)
        }
    }

    // this function handles a division that may not succeed
    fn try_division(divident: i32, divisor: i32) {
        match checked_division(divident, divisor) {
            None => println!("{} / {} failed!", divident, divisor),
            Some(quotient) => {
                println!("{} / {} = {}", divident, divisor, quotient)
            },
        }
    }

    try_division(4, 2);
    try_division(1, 0);

    // binding none to a variable needs to be type annotated
    let none: Option<i32> = None;
    let _equivalent_none = None::<i32>;

    let optional_float = Some(0_f32);

    println!("{:?} unwraps to {:?}", optional_float, optional_float.unwrap());

    // panic!
    // println!("{:?} unwraps to {:?}", none, none.unwrap());
}
