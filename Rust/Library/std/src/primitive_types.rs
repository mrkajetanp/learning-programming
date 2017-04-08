pub fn primitive_types_main() {
    println!("///*** Primitive Types ***\\\\\\\n");

    arrays();
    bools();
    chars();

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

fn chars() {
    println!("*** Chars ***");

    use std;

    // char is always 4 bytes in size
    let v = vec!['h', 'e', 'l', 'l', 'o'];

    assert_eq!(20, v.len() * std::mem::size_of::<char>());

    let s = String::from("hello");

    // five elements, one byte per element
    assert_eq!(5, s.len() * std::mem::size_of::<u8>());

    assert!('1'.is_digit(10));
    assert!('f'.is_digit(16));
    assert!(!'f'.is_digit(10));

    assert_eq!('1'.to_digit(10), Some(1));
    assert_eq!('f'.to_digit(16), Some(15));

    assert_eq!('f'.to_digit(10), None);
    assert_eq!('z'.to_digit(16), None);

    println!("Unicode escape sequence for X: {}", 'X'.escape_unicode());

    // experimental api
    println!("Debug escape for \\n: {}", '\n'.escape_debug());

    println!("default escape for '\"': {}", '"'.escape_default());

    assert_eq!(1, 'A'.len_utf8());
    assert_eq!(2, 'ß'.len_utf8());

    assert_eq!(1, 'ß'.len_utf16());

    // encoding as utfs

    let mut b = [0; 2];
    let result = 'ß'.encode_utf8(&mut b);

    assert_eq!(result, "ß");
    assert_eq!(result.len(), 2);

    assert!('a'.is_alphabetic());

    assert!('a'.is_lowercase());
    assert!('ß'.is_lowercase());

    assert!('A'.is_uppercase());

    assert!(' '.is_whitespace());
    assert!(!'x'.is_whitespace());

    assert!('x'.is_alphanumeric());
    assert!('1'.is_alphanumeric());

    assert!('\r'.is_control());
    assert!(!'x'.is_control());

    assert!('8'.is_numeric());
    assert!(!'a'.is_numeric());

    println!("'X' to lowercase is: {}", 'X'.to_lowercase());
    assert_eq!('C'.to_lowercase().to_string(), "c");

    println!("'a' to uppercase is: {}", 'a'.to_uppercase());
    assert_eq!('a'.to_uppercase().to_string(), "A");

    println!("");
}
