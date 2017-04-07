pub fn primitive_types_main() {
    println!("///*** Primitive Types ***\\\\\\\n");

    arrays();
    bools();

    println!("");
}

fn arrays() {
    println!("*** Arrays ***");

    let mut array: [i32; 3] = [0; 3];

    array[1] = 1;
    array[2] = 2;

    assert_eq!([1, 2], &array[1..]);

    // only if array has 32 of fever elements
    for x in &array {
        print!("{} ", x);
    }
    println!("");

    // any size
    for x in array.iter() {
        print!("{} ", x);
    }
    println!("");

    println!("");
}

fn bools() {
    println!("*** Bools ***");

    // binary operations
    let bool_val = true & false | false;
    assert!(!bool_val);

    // simple usage

    let praise_the_borrow_checker = true;

    if praise_the_borrow_checker {
        println!("yeah!");
    } else {
        println!("what?!");
    }

    match praise_the_borrow_checker {
        true => println!("keep praising!"),
        false => println!("you should praise!"),
    }

    // casting to a number

    assert_eq!(true as i32, 1);
    assert_eq!(false as i32, 0);

    // bool from a string

    use std::str::FromStr;

    assert_eq!(FromStr::from_str("true"), Ok(true));
    assert_eq!(FromStr::from_str("false"), Ok(false));
    assert!(<bool as FromStr>::from_str("not even a bool").is_err());

    assert_eq!("true".parse(), Ok(true));
    assert_eq!("false".parse(), Ok(false));
    assert!("not even a bool".parse::<bool>().is_err());

    println!("");
}
