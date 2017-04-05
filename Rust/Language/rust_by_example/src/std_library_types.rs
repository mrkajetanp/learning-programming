pub fn std_library_types() {
    println!("***STD Library Types***");

    box_stack_heap();
    vectors();
    strings();
    options();
    results();
    panics();
    hash_map();

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

fn results() {
    /* basics */
    {
        mod checked {
            // Mathematical "errors" we want to catch
            #[derive(Debug)]
            pub enum MathError {
                DivisionByZero,
                NonPositiveLogarithm,
                NegativeSquareRoot,
            }

            pub type MathResult = Result<f64, MathError>;

            pub fn div(x: f64, y: f64) -> MathResult {
                if y == 0.0 {
                    Err(MathError::DivisionByZero)
                } else {
                    Ok(x / y)
                }
            }

            pub fn sqrt(x: f64) -> MathResult {
                if x < 0.0 {
                    Err(MathError::NegativeSquareRoot)
                } else {
                    Ok(x.sqrt())
                }
            }

            pub fn ln(x: f64) -> MathResult {
                if x <= 0.0 {
                    Err(MathError::NonPositiveLogarithm)
                } else {
                    Ok(x.ln())
                }
            }
        }

        fn op(x: f64, y: f64) -> f64 {
            match checked::div(x, y) {
                Err(why) => panic!("{:?}", why),
                Ok(ratio) => match checked::ln(ratio) {
                    Err(why) => panic!("{:?}", why),
                    Ok(ln) => match checked::sqrt(ln) {
                        Err(why) => panic!("{:?}", why),
                        Ok(sqrt) => sqrt,
                    },
                },
            }
        }
    }

    // println!("{}", op(1.0, 10.0));

    /* try! */
    {
        mod checked {
            #[derive(Debug)]
            enum MathError {
                DivisionByZero,
                NegativeLogarithm,
                NegativeSquareRoot,
            }

            type MathResult = Result<f64, MathError>;

            fn div(x: f64, y: f64) -> MathResult {
                if y == 0.0 {
                    Err(MathError::DivisionByZero)
                } else {
                    Ok(x / y)
                }
            }

            fn sqrt(x: f64) -> MathResult {
                if x < 0.0 {
                    Err(MathError::NegativeSquareRoot)
                } else {
                    Ok(x.sqrt())
                }
            }

            fn ln(x: f64) -> MathResult {
                if x < 0.0 {
                    Err(MathError::NegativeSquareRoot)
                } else {
                    Ok(x.ln())
                }
            }

            // intermediate function
            fn op_(x: f64, y: f64) -> MathResult {
                // if division fails, then DivisionByZero will be returned
                let ratio = try!(div(x, y));

                // if ln fails, the NegativeLogarithm will be returned
                let ln = try!(ln(ratio));

                sqrt(ln)
            }

            pub fn op(x: f64, y: f64) {
                match op_(x, y) {
                    Err(why) => panic!(match why {
                        MathError::NegativeLogarithm
                            => "Logarithm of a negative number.",
                        MathError::DivisionByZero
                            => "Division by zero",
                        MathError::NegativeSquareRoot
                            => "square root of a negative number",
                    }),
                    Ok(value) => println!("{}", value),
                }
            }
        }
        // checked::op(1.0, 10.0);
    }
}

fn panics() {
    // panic unwinds current thread's stack so it'll free all of its resources
    fn divison(divident: i32, divisor: i32) -> i32 {
        if divisor == 0 {
            panic!("division by zero");
        } else {
            divident / divisor
        }
    }

    let _x = Box::new(0_i32);

    // this will panic
    // divison(3, 0);

    // this point won't be reached

    // _x should be destroyed at this point
}

fn hash_map() {
    /* basics */
    {
        use std::collections::HashMap;

        fn call(number: &str) -> &str {
            match number {
                "798-1364" => "Sorry, this call could not be completed",
                "645-7689" => "Hello, pizza here.",
                _ => "Who is this again?",
            }
        }

        let mut contacts = HashMap::new();

        contacts.insert("Daniel", "798-1364");
        contacts.insert("Ashley", "645-7689");
        contacts.insert("Katie", "435-8291");
        contacts.insert("Robert", "956-1745");

        // Takes a reference and return Option<&V>
        match contacts.get(&"Daniel") {
            Some(&number) => println!("Calling Daniel: {}", call(number)),
            _ => println!("Don't have Daniel's number."),
        }

        // HashMap::insert() return None
        // if the inserted value is now, Some(value) otherwise
        contacts.insert("Daniel", "164-6743");

        match contacts.get(&"Ashley") {
            Some(&number) => println!("Calling Ashley: {}", call(number)),
            _ => println!("Don't have Ashley's number."),
        }

        contacts.remove(&"Ashley");

        println!("> Iter now:");

        // HashMap::iter() return an iterator that yields
        // (&'a key, &'a value) pairs in arbitrary order
        for (contact, &number) in contacts.iter() {
            println!("Calling {}: {}", contact, call(number));
        }
    }

    /* custom key types */
    {
        use std::collections::HashMap;

        #[derive(PartialEq, Eq, Hash)]
        struct Account<'a> {
            username: &'a str,
            password: &'a str,
        }

        struct AccountInfo<'a> {
            name: &'a str,
            email: &'a str,
        }

        type Accounts<'a> = HashMap<Account<'a>, AccountInfo<'a>>;

        fn try_logon<'a>(accounts: &Accounts<'a>,
                         username: &'a str, password: &'a str) {
            println!("Username: {}", username);
            println!("Password: {}", password);
            println!("Attempting logon..");

            let logon = Account {
                username: username,
                password: password,
            };

            match accounts.get(&logon) {
                Some(account_info) => {
                    println!("Successful logon!");
                    println!("Name: {}", account_info.name);
                    println!("Email: {}", account_info.email);
                },
                _ => println!("Login failed"),
            }
        }

        let mut accounts: Accounts = HashMap::new();

        let account = Account {
            username: "j.everyman",
            password: "password123",
        };

        let account_info = AccountInfo {
            name: "John Everyman",
            email: "j.everyman@email.com",
        };

        accounts.insert(account, account_info);

        try_logon(&accounts, "j.everyman", "psawword123");
        try_logon(&accounts, "j.everyman", "password123");
    }

    /* hash set */
    {
        use std::collections::HashSet;

        let mut a: HashSet<i32> = vec!(1_i32, 2, 3).into_iter().collect();
        let mut b: HashSet<i32> = vec!(2_i32, 3, 4).into_iter().collect();

        assert!(a.insert(4));
        assert!(a.contains(&4));

        // HashSet::insert() returns false
        // if there was a value already present
        // assert!(b.insert(4), "Value 4 is already in set B");

        b.insert(5);

        // if a collection's element type implement Debug
        // then the collection implements Debug
        println!("A: {:?}", a);
        println!("B: {:?}", b);

        println!("Union: {:?}", a.union(&b).collect::<Vec<&i32>>());

        println!("Difference: {:?}", a.difference(&b).collect::<Vec<&i32>>());

        println!("Intersection: {:?}", a.intersection(&b).collect::<Vec<&i32>>());

        println!("Symmetric difference: {:?}",
                 a.symmetric_difference(&b).collect::<Vec<&i32>>());
    }
}
