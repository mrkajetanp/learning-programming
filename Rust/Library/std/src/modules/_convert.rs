pub fn convert() {

    as_ref();
    as_mut();
    from();
    into();

    // try_from
    // try_into
}


fn as_ref() {
    // reference-to-reference conversion

    fn is_hello<T: AsRef<str>>(s: T) {
        assert_eq!("hello", s.as_ref());
    }

    let s = "hello";
    is_hello(s);

    let s = "hello".to_string();
    is_hello(s);
}

fn as_mut() {
    // mut-ref to mut-ref conversion

    fn add_one<T: AsMut<u64>>(num: &mut T) {
        *num.as_mut() += 1;
    }

    let mut boxed_num = Box::new(0);

    add_one(&mut boxed_num);
    assert_eq!(*boxed_num, 1);
}

fn from() {
    // constructs self via conversion

    let string = "hello".to_string();
    let other_string = String::from("hello");

    assert_eq!(string, other_string);
}

fn into() {
    // conversion consuming self

    fn is_hello<T: Into<Vec<u8>>>(s: T) {
        let bytes = b"hello".to_vec();
        assert_eq!(bytes, s.into());
    }

    let s = "hello".to_string();
    is_hello(s);
}
