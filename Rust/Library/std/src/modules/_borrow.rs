pub fn _borrow() {
    println!("*** Borrow ***", );

    cow();
    borrow_trait();
    to_owned_trait();

    println!("", );
}

fn cow() {
    use std::borrow::Cow;

    fn abs_all(input: &mut Cow<[i32]>) {
        for i in 0..input.len() {
            let v = input[i];
            if v < 0 {
                // clones into a vector if not already owned
                input.to_mut()[i] = -v;
            }
        }
    }

    // no clone occurs because input doesn't need to be mutated
    let slice = [0, 1, 2];
    let mut input = Cow::from(&slice[..]);
    abs_all(&mut input);

    println!("slice: {:?}\ninput: {:?}", slice, input);

    // clone occurs because input needs to be mutated
    let slice = [-1, 0, 1];
    let mut input = Cow::from(&slice[..]);
    abs_all(&mut input);

    println!("slice: {:?}\ninput: {:?}", slice, input);

    // no clone occurs because input is already owned
    let mut input = Cow::from(vec![-1, 0, 1]);
    abs_all(&mut input);

    println!("input: {:?}", input);

    // into_owned

    let cow: Cow<[_]> = Cow::Owned(vec![1, 2, 3]);
    let hello = cow.into_owned();

    // cow has been moved

    assert_eq!(vec![1, 2, 3], hello);
}

fn borrow_trait() {
    use std::borrow::Borrow;

    fn check<T: Borrow<str>>(s: T) {
        assert_eq!("Hello", s.borrow());
    }

    let s = "Hello".to_string();
    check(s);

    let s = "Hello";
    check(s);

    // there is borrow_mut as well
}

fn to_owned_trait() {
    let s = "a"; // str
    let ss = s.to_owned(); // String

    let v = &[1, 2]; // slice
    let vv = v.to_owned(); // vector
}
