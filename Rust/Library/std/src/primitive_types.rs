pub fn primitive_types_main() {
    println!("///*** Primitive Types ***\\\\\\\n");

    arrays();
    bools();
    chars();
    f32s();
    i32s();
    raw_pointers();
    slices();
    strs();
    tuples();
    u32s();

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

fn raw_pointers() {
    /* creating raw pointers */

    // coercion
    let my_num: i32 = 10;
    let my_num_ptr: *const i32 = &my_num;

    let mut my_speed: i32 = 88;
    let my_speed_ptr: *mut i32 = &mut my_speed;

    // ptr to a boxed value

    let my_num: Box<i32> = Box::new(10);
    let my_num_ptr: *const i32 = &*my_num;

    let mut my_speed: Box<i32> = Box::new(88);
    let my_speed_ptr: *mut i32 = &mut *my_speed;

    // consuming a box

    let my_speed: Box<i32> = Box::new(99);
    let my_speed: *mut i32 = Box::into_raw(my_speed);

    // we took ownership, so we have to destroy it
    unsafe {
        drop(Box::from_raw(my_speed));
    }

    // pointer from C

    use std::mem;
    use libc;

    unsafe {
        let my_num: *mut i32 = libc::malloc(mem::size_of::<i32>()) as *mut i32;
        *my_num = 7;
        if my_num.is_null() {
            panic!("failed to allocate memory!");
        }
        println!("my_num points to: {}", *my_num);
        libc::free(my_num as *mut libc::c_void);
    }

    let s: &str = "follow the rabbit";
    let ptr: *const u8 = s.as_ptr();
    assert!(!ptr.is_null());

    let val: *const u8 = &10_u8 as *const u8;

    unsafe {
        if let Some(val_back) = val.as_ref() {
            println!("we got back the value: {}", val_back);
        }
    }

    // offset from a pointer in units

    let s: &str = "123";
    let ptr: *const u8 = s.as_ptr();
    unsafe {
        println!("{}", *ptr.offset(0) as char);
        println!("{}", *ptr.offset(1) as char);
        println!("{}", *ptr.offset(2) as char);
    }

    // there is a wrapping offset, too

    let mut s = [1, 2, 3];
    println!("s: {:?}", s);
    let ptr: *mut u32 = s.as_mut_ptr();
    let first_value = unsafe { ptr.as_mut().unwrap() };
    *first_value = 4;
    let second_value = unsafe { ptr.offset(2).as_mut().unwrap() };
    *second_value = 8;
    println!("s: {:?}", s);

}

fn slices() {
    let v1 = vec![1, 2, 3];
    let int_slice = &v1[..];

    let str_slice: &[&str] = &["one", "two", "three"];

    println!("{:?}", str_slice);

    let x = &mut [1, 2, 3];
    x[1] = 7;
    assert_eq!(x, &[1, 7, 3]);

    let a = [1, 2, 3];

    assert_eq!(3, a.len());
    assert!(!a.is_empty());

    assert_eq!(Some(&1), a.first());

    let x = &mut [0, 1, 2];

    if let Some(first) = x.first_mut() {
        *first = 5;
    }
    assert_eq!(&[5, 1, 2], x);

    let x = &[0, 1, 2];
    if let Some((first, elems)) = x.split_first() {
        assert_eq!(&0, first);
        assert_eq!(&[1, 2], elems);
    }

    // x.split_first_mut()

    if let Some((last, elems)) = x.split_last() {
        assert_eq!(&2, last);
        assert_eq!(&[0, 1], elems);
    }

    // x.split_last_mut()

    assert_eq!(Some(&2), x.last());

    // x.last_mut()

    let v = [10, 40, 30];
    assert_eq!(Some(&40), v.get(1));
    assert_eq!(Some(&[10, 40][..]), v.get(0..2));
    assert_eq!(None, v.get(3));
    assert_eq!(None, v.get(0..4));

    // x.get_mut(2)

    // x.get_unchecked(2)
    // x.get_unchecked_mut(2)

    let x = &[1, 2, 4];
    let x_ptr = x.as_ptr();

    unsafe {
        for i in 0..x.len() {
            assert_eq!(x.get_unchecked(i), &*x_ptr.offset(i as isize));
        }
    }

    // x.as_mut_ptr()

    let mut v = ["a", "b", "c"];
    v.swap(0, 2);
    assert_eq!(["c", "b", "a"], v);

    let mut v = [1, 2, 3];
    v.reverse();
    println!("{:?}", v);

    let x = &[1, 2, 4];
    let mut iterator = x.iter();

    assert_eq!(Some(&1), iterator.next());
    assert_eq!(Some(&2), iterator.next());
    assert_eq!(Some(&4), iterator.next());
    assert_eq!(None, iterator.next());

    // x.iter_mut()

    let sl = ['r', 'u', 's', 't'];
    let mut iter = sl.windows(2);
    assert_eq!(&['r', 'u'], iter.next().unwrap());
    assert_eq!(&['u', 's'], iter.next().unwrap());
    assert_eq!(&['s', 't'], iter.next().unwrap());
    assert!(iter.next().is_none());

    let slice = ['l', 'o', 'r', 'e', 'm'];
    let mut iter = slice.chunks(2);

    assert_eq!(&['l', 'o'], iter.next().unwrap());
    assert_eq!(&['r', 'e'], iter.next().unwrap());
    assert_eq!(&['m'], iter.next().unwrap());

    // x.chunks_mut(2);

    let v = [10, 40, 30, 20, 50];
    let (v1, v2) = v.split_at(2);

    assert_eq!([10, 40], v1);
    assert_eq!([30, 20, 50], v2);

    // v.split_at_mut(2);

    let slice = [10, 40, 33, 20];
    let mut iter = slice.split(|num| num % 3 == 0);

    assert_eq!(&[10, 40], iter.next().unwrap());
    assert_eq!(&[20], iter.next().unwrap());
    assert!(iter.next().is_none());

    // let mut iter = slice.split_mut(|num| num % 3 == 0);

    // let mut iter = slice.splitn(2, |num| num % 3 == 0);
    // let mut iter = slice.splitn_mut(2, |num| num % 3 == 0);

    // let mut iter = slice.rsplitn(2, |num| num % 3 == 0);
    // let mut iter = slice.rsplitn_mut(2, |num| num % 3 == 0);

    let v = [10, 40, 30];
    assert!(v.contains(&30));
    assert!(!v.contains(&50));

    let v = vec![10, 20, 30];
    assert!(v.contains(&30));
    assert!(!v.contains(&50));

    assert!(v.starts_with(&[10]));
    assert!(v.starts_with(&[10, 20]));

    // v.ends_with()

    // v.binary_search(&20);
    // v.binary_search_by(|x| fn..);
    // v.binary_search_by_key(..);

    let mut v = [-2, 4, 1, -5, 3];
    v.sort();
    println!("sorted v: {:?}", v);

    // v.sort_by_key(|k| k.abs());
    // v.sort_by(|a, b| a.cmp(b));
    // v.sort_by(|a, b| b.cmp(a));

    let mut dst = [0, 0, 0];
    let src = [1, 2, 3];

    dst.clone_from_slice(&src);
    assert!(dst == [1, 2, 3]);

    let mut dst = [0, 0, 0];
    dst.copy_from_slice(&src);
    assert_eq!(src, dst);

    let s = [10, 40, 50];
    let x = s.to_vec();
    // s and x can be modified independently

    let s: Box<[i32]> = Box::new([10, 40, 30]);
    let x = s.into_vec();
    // s cannot be used anymore, it's been converted

    assert_eq!(x , vec![10, 40, 30]);
}

fn strs() {
    use std::slice;
    use std::str;

    let story = "Once upon a time..";
    let ptr = story.as_ptr();
    let len = story.len();

    assert_eq!(18, len);

    // we can re-build a str out of ptr and len
    let s = unsafe {
        // build a &[u8]
        let slice = slice::from_raw_parts(ptr, len);

        // .. and convert that slice into a string slice
        str::from_utf8(slice)
    };

    assert_eq!(Ok(story), s);

    let s = "";
    assert!(s.is_empty());

    let s = "test";
    assert!(s.is_char_boundary(1));
    assert!(s.is_char_boundary(2));

    let bytes = "bors".as_bytes();
    assert_eq!(b"bors", bytes);


    let s = "Hello, world!";
    unsafe {
        assert_eq!("world", s.slice_unchecked(7, 12));
        // assert_eq!("world", s.slice_mut_unchecked(7, 12));
    }

    let s = "Cajetan Puchalski";
    let (name, surname) = s.split_at(7);
    // let (name, surname) = s.split_at_mut(7);

    assert_eq!("Cajetan", name);
    assert_eq!(" Puchalski", surname);

    let test = "a test";

    for c in test.chars() {
        print!("{} ", c);
    }
    println!("");

    for (i, c) in test.char_indices() {
        println!("{}: {}", i, c);
    }

    for b in test.bytes() {
        println!("{} ", b);
    }
    println!("");

    let sentence = "let's test me";
    for word in sentence.split_whitespace() {
        println!("{}", word);
    }

    let paragraph = "lorem ipsum\ndolor sit amet\nconsectetur adipiscing elit";
    for line in paragraph.lines() {
        println!("{}", line);
    }

    for s in test.encode_utf16() {
        print!("{} ", s);
    }
    println!("");

    let bananas = "bananas";
    assert!(bananas.contains("nana"));
    assert!(!bananas.contains("apples"));

    assert!(bananas.starts_with("ban"));
    assert!(bananas.ends_with("nas"));

    println!("{:?}", bananas.find('n'));
    println!("{:?}", bananas.rfind('n'));

    let v: Vec<&str> = "Mary had a little lamb".split(' ').collect();
    println!("{:?}", v);

    let v: Vec<&str> = "lionXtigerXleopard".split(char::is_uppercase).collect();
    println!("{:?}", v);

    let v: Vec<&str> = "abc1defXghi".split(|c| c == '1' || c == 'X').collect();
    println!("{:?}", v);

    let v: Vec<&str> = "Mary had a little lamb".rsplit(' ').collect();
    println!("{:?}", v);

    let v: Vec<&str> = "A.B.".split_terminator('.').collect();
    println!("{:?}", v);

    // let v: Vec<&str> = "A..B..".split_terminator('.').collect();
    // println!("{:?}", v);

    // let v: Vec<&str> = "A.B.".rsplit_terminator('.').collect();
    // println!("{:?}", v);

    // splitn
    // rsplitn

    let v: Vec<&str> = "abcXXXabcYYYabc".matches("abc").collect();
    println!("{:?}", v);

    let v: Vec<&str> = "1abc2abc3".matches(char::is_numeric).collect();
    println!("{:?}", v);

    // rmatches

    // match_indices
    // rmatch_indices

    let test = "   hehehe   ";
    println!("test: |{}|", test);
    println!("test: |{}|", test.trim());

    // trim_left
    // trim_right

    let test2 = "111test1test111";
    println!("test2 trimmed: {}", test2.trim_matches('1'));

    // trim_left_matches
    // trim_right_matches

    let four: u32 = "4".parse().unwrap();
    assert_eq!(4, four);

    let four = "4".parse::<u32>();
    assert_eq!(Ok(4), four);

    let phr = "this is old";
    assert_eq!("this is new", phr.replace("old", "new"));

    // replacen

    // to_lowercase
    // to_uppercase

    let string = String::from("birthday gift");
    let boxed_str = string.clone().into_boxed_str();

    assert_eq!(boxed_str.into_string(), string);

    assert_eq!("abc".repeat(2), String::from("abcabc"));
}

fn tuples() {
    fn point() -> (i32, i32) {
        (4, 5)
    }

    let pt = point();
    assert_eq!(4, pt.0);
    assert_eq!(5, pt.1);

    let (x, y) = point();

    assert_eq!(4, x);
    assert_eq!(5, y);
}

fn u32s() {
    assert!(16_u8.is_power_of_two());

    assert_eq!(2, 2_u8.next_power_of_two());
    assert_eq!(2, 2_u8.next_power_of_two());

    assert_eq!(Some(2), 2_u8.checked_next_power_of_two());
    assert_eq!(Some(4), 3_u8.checked_next_power_of_two());
    assert_eq!(None, 200_u8.checked_next_power_of_two());
}
