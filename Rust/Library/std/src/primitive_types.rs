pub fn primitive_types_main() {
    println!("///*** Primitive Types ***\\\\\\\n");

    arrays();
    bools();
    chars();
    f32s();
    i32s();

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

fn f32s() {
    println!("*** f32 ***");

    use std::f32;

    let nan = f32::NAN;
    assert!(nan.is_nan());
    assert!(!89_f32.is_nan());

    let inf = f32::INFINITY;
    let n_inf = f32::NEG_INFINITY;

    assert!(!89_f32.is_infinite());
    assert!(!nan.is_infinite());

    assert!(inf.is_infinite());
    assert!(n_inf.is_infinite());

    assert!(89_f32.is_finite());
    assert!(!inf.is_finite());
    assert!(!n_inf.is_finite());
    assert!(!nan.is_finite());

    let min = f32::MIN_POSITIVE;
    let max = f32::MAX;

    let lower_than_min = 1.0e-40_f32;
    let zero = 0.0_f32;

    assert!(min.is_normal());
    assert!(max.is_normal());

    assert!(!zero.is_normal());
    assert!(!f32::NAN.is_normal());
    assert!(!f32::INFINITY.is_normal());

    // values between 0 and min are Subnormal
    assert!(!lower_than_min.is_normal());

    use std::num::FpCategory;

    assert_eq!(12.4_f32.classify(), FpCategory::Normal);
    assert_eq!(f32::INFINITY.classify(), FpCategory::Infinite);

    assert_eq!(3.99_f32.floor(), 3.0);

    assert_eq!(3.01_f32.ceil(), 4.0);

    assert_eq!(3.3_f32.round(), 3.0);
    assert_eq!(3.5_f32.round(), 4.0);

    assert_eq!(3.3_f32.trunc(), 3.0);
    assert_eq!(3.9_f32.trunc(), 3.0);

    assert_eq!(3.5_f32.fract(), 0.5);
    assert_eq!(3.0_f32.fract(), 0.0);

    assert_eq!(3.5_f32.abs(), 3.5);
    assert_eq!((-3.5_f32).abs(), 3.5);

    assert_eq!(1.0, 3.5_f32.signum());
    assert_eq!(-1.0, (-3.5_f32).signum());

    assert!(f32::NAN.signum().is_nan());

    assert!(7.0_f32.is_sign_positive());
    assert!(!(-7.0_f32).is_sign_positive());

    assert!((-7.0_f32).is_sign_negative());
    assert!(!7.0_f32.is_sign_negative());

    assert!(!f32::NAN.is_sign_positive() && !f32::NAN.is_sign_negative());

    println!("10.0 * 2.0 + 0.5 = {}", 10.0_f32.mul_add(2.0, 0.5));

    assert_eq!(0.25, 4.0_f32.recip());

    assert_eq!(32.0, 2.0_f32.powi(5));

    assert_eq!(32.0, 2.0_f32.powf(5.0));

    assert_eq!(4.0, 16.0_f32.sqrt());

    println!("e^2 = {}", 2.0_f32.exp());

    println!("2^6 = {}", 6.0_f32.exp2());

    // natural logarithm
    println!("ln(10) = {}", 10.0_f32.ln());

    println!("log(10, 2) = {}", 10.0_f32.log(2.0));

    println!("log2(10) = {}", 10.0_f32.log2());

    println!("log10(20) = {}", 20.0_f32.log10());

    println!("pi to degrees: {}", f32::consts::PI.to_degrees());

    println!("180 deg to radians: {}", 180.0_f32.to_radians());

    assert_eq!(3.0, 1.0_f32.max(3.0));
    assert_eq!(1.0, 1.0_f32.min(3.0));

    assert_eq!(3.0, 27.0_f32.cbrt());

    println!("hypot of 2.0 and 3.0 = {}", 2.0_f32.hypot(3.0));

    // radians
    println!("sin of 3: {}", 3.0_f32.sin());
    println!("cos of 3: {}", 3.0_f32.cos());
    println!("tan of 3: {}", 3.0_f32.tan());

    println!("asin of 0.8: {}", 0.8_f32.asin());
    println!("acos of 0.8: {}", 0.8_f32.acos());
    println!("atan of 0.8: {}", 0.8_f32.atan());
    println!("atan2 of 3.0 and 0.5: {}", 3.0_f32.atan2(0.5));

    println!("sin and cos of 3: {:?}", 3.0_f32.sin_cos());

    println!("e^2-1 is {}", 2.0_f32.exp_m1());

    println!("ln_1p of 2.5: {}", 2.5_f32.ln_1p());

    println!("sinh of 3: {}", 3.0_f32.sinh());
    println!("cosh of 3: {}", 3.0_f32.cosh());
    println!("tanh of 3: {}", 3.0_f32.tanh());

    println!("asinh of 3: {}", 3.0_f32.asinh());
    println!("acosh of 3: {}", 3.0_f32.acosh());
    println!("atanh of 0.8: {}", 0.8_f32.atanh());

    println!("");
}

fn i32s() {
    println!("*** i32s ***");

    assert_eq!(i8::min_value(), -128);
    assert_eq!(i8::max_value(), 127);

    assert_eq!(i32::from_str_radix("A", 16), Ok(10));

    assert_eq!(1, (-0b1000_0000i8).count_ones());
    assert_eq!(7, (-0b1000_0000i8).count_zeros());

    assert_eq!(0, (-1_i16).leading_zeros());
    assert_eq!(2, (-4_i8).trailing_zeros());

    println!("{:b}", (0b1001010_i64).rotate_left(1));
    println!("{:b}", (0b1001010_i64).rotate_right(1));

    println!("{:x}", 0x01234567i64.swap_bytes());

    assert_eq!(Some(32767), 7_i16.checked_add(32760));
    assert_eq!(None, 8_i16.checked_add(32760));

    assert_eq!(Some(-128), (-127_i8).checked_sub(1));
    assert_eq!(None, (-128_i8).checked_sub(1));

    assert_eq!(Some(126), 6_i8.checked_mul(21));
    assert_eq!(Some(127), (-127_i8).checked_div(-1));
    assert_eq!((1_i8).checked_div(0), None);

    assert_eq!(Some(1), 5_i32.checked_rem(2));
    assert_eq!(None, 5_i32.checked_rem(0));

    use std::i32;

    assert_eq!(Some(-5), 5_i32.checked_neg());
    assert_eq!(None, i32::MIN.checked_neg());

    assert_eq!(Some(0x100), 0x10_i32.checked_shl(4));
    assert_eq!(Some(0x1), 0x10_i32.checked_shr(4));

    assert_eq!(Some(5), (-5_i32).checked_abs());

    assert_eq!(100_i8.saturating_add(1), 101);
    assert_eq!(100_i8.saturating_add(127), 127);

    // there are saturating variants for everything else too

    assert_eq!(100_i8.wrapping_add(27), 127);
    assert_eq!(100_i8.wrapping_add(127), -29);

    // there are wrapping variants for everything else too

    assert_eq!((7, false), 5_i32.overflowing_add(2));
    assert_eq!((i32::MIN, true), i32::MAX.overflowing_add(1));

    // there are overflowing variants for everything else too

    assert_eq!(16, 2_i32.pow(4));
    assert_eq!(7, 7_i32.abs());
    assert_eq!(7, (-7_i32).abs());

    assert_eq!(1, 10_i8.signum());
    assert_eq!(0, 0_i8.signum());
    assert_eq!(-1, (-10_i8).signum());

    assert!(10_i8.is_positive());

    assert!((-10_i8).is_negative());

    println!("");
}
