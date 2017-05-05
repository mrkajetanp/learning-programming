pub fn string() {
    basics();
    string_struct();
    representation();
}

fn basics() {
    let s = "Hello".to_string();
    let s = String::from("world");
    let s: String = "also this".into();

    let message = s + " world!";

    let sparkle_heart = vec![240, 159, 146, 150];
    let sparkle_heart = String::from_utf8(sparkle_heart).unwrap();

    let bytes = sparkle_heart.into_bytes();
}

fn string_struct() {
    let mut hello = String::from("Hello, ");
    hello.push('w');
    hello.push_str("orld!");

    // deref
    fn takes_str(s: &str) { }
    let s = String::from("Hello");
    takes_str(&s);
}

fn representation() {
    use std::mem;

    let story = String::from("Once upon a time...");

    let ptr = story.as_ptr();
    let len = story.len();
    let capacity = story.capacity();

    // story has 19 bytes
    assert_eq!(19, len);

    // we have all the parts we need
    mem::forget(story);

    let s = unsafe { String::from_raw_parts(ptr as *mut _, len, capacity) };
    assert_eq!(String::from("Once upon a time..."), s);

    let mut s = String::new();

    println!("{}", s.capacity());

    for _ in 0..5 {
        s.push_str("hello");
        println!("{}", s.capacity());
    }

    println!("");

    let mut s = String::with_capacity(25);
    println!("{}", s.capacity());

    for _ in 0..5 {
        s.push_str("hello");
        println!("{}", s.capacity());
    }
}
