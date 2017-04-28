pub fn _option() {
    basics();
    options_and_pointers();

    basics_2();
    advanced();
    option_struct();
}

fn basics() {
    fn divide(numerator: f64, denominator: f64) -> Option<f64> {
        if denominator == 0.0 {
            None
        } else {
            Some(numerator / denominator)
        }
    }

    // return value is optional
    let result = divide(2.0, 3.0);

    // pattern match to retrieve the value
    match result {
        // the division was valid
        Some(x) => println!("Result: {}", x),
        // the division was invalid
        None => println!("Cannot divide by 0"),
    }
}

fn options_and_pointers() {

    fn check_optional(optional: &Option<Box<i32>>) {
        match *optional {
            Some(ref p) => println!("has value {}", p),
            None => println!("has no value"),
        }
    }

    let optional: Option<Box<i32>> = None;
    check_optional(&optional);

    let optional: Option<Box<i32>> = Some(Box::new(1234));
    check_optional(&optional);
}

fn basics_2() {
    let msg = Some("test message");

    if let Some(ref m) = msg {
        println!("{}", *m);
    }

    let unwrapped_msg = msg.unwrap_or("default");
}

fn advanced() {
    enum Kingdom {
        Plant(u32, &'static str),
        Animal(u32, &'static str)
    }

    let all_the_big_things = [
        Kingdom::Plant(250, "redwood"),
        Kingdom::Plant(230, "sugar pine"),
        Kingdom::Animal(25, "blue whale"),
        Kingdom::Animal(19, "fin whale"),
    ];

    let mut name_of_biggest_animal = None;
    let mut size_of_biggest_animal = 0;

    for big_thing in &all_the_big_things {
        match *big_thing {
            // match guard
            Kingdom::Animal(size, name) if size > size_of_biggest_animal => {
                size_of_biggest_animal = size;
                name_of_biggest_animal = Some(name);
            }
            Kingdom::Animal(..) | Kingdom::Plant(..) => ()
        }
    }

    match name_of_biggest_animal {
        Some(name) => println!("the biggest animal is {}", name),
        None => println!("there are no animals.."),
    }
}

fn option_struct() {
    let x: Option<u32> = Some(2);
    assert_eq!(true, x.is_some());

    let x: Option<u32> = None;
    assert_eq!(true, x.is_none());

    let num_as_str: Option<String> = Some("10".to_string());
    assert_eq!("10", *num_as_str.as_ref().unwrap());

    let mut x = Some(2);
    match x.as_mut() {
        Some(v) => *v = 42,
        None => {},
    }
    assert_eq!(Some(42), x);

    let x = Some("value");
    // expect panics if value is none
    assert_eq!("value", x.expect("the world is ending"));
    assert_eq!("bike", None.unwrap_or("bike"));

    let k = 10;
    assert_eq!(20, None.unwrap_or_else(|| 2 * k));

    let maybe_some_string = Some(String::from("Hello, World!"));
    let maybe_some_len = maybe_some_string.map(|s| s.len());
    assert_eq!(Some(13), maybe_some_len);

    // map_or
    // map_or_else

    // *** Converting Option to Result ***
    // ok_or
    // ok_or_else

    // iter
    // iter_mut

    // and
    // and_then

    // or
    // or_else

    // get_or_insert
    // get_or_insert_with

    // take
    // cloned - Option<&T> into Option<T>

    // unwrap_or_default
    // into_iter
    // from_iter
}
