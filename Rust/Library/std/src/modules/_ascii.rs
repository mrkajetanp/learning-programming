pub fn _ascii() {
    println!("*** ascii ***", );

    escaping();
    ascii_ext_trait();
    println!("", );
}

fn escaping() {
    use std::ascii;

    let esc =  ascii::escape_default(b'0').next().unwrap();
    assert_eq!(b'0', esc);

    let mut esc = ascii::escape_default(b'\t');
    assert_eq!(b'\\', esc.next().unwrap());
    assert_eq!(b't', esc.next().unwrap());
}

fn ascii_ext_trait() {
    use std::ascii::AsciiExt;

    assert_eq!("cafe".to_ascii_uppercase(), "CAFE");

    assert!('a'.is_ascii());
    assert!(!'é'.is_ascii());

    assert_eq!('A', 'a'.to_ascii_uppercase());
    assert_eq!('a', 'A'.to_ascii_lowercase());

    assert!('a'.eq_ignore_ascii_case(&'A'));
    assert!('A'.eq_ignore_ascii_case(&'A'));
    assert!('A'.eq_ignore_ascii_case(&'a'));

    let mut ascii = 'a';

    ascii.make_ascii_uppercase();
    assert_eq!('A', ascii);

    ascii.make_ascii_lowercase();
    assert_eq!('a', ascii);
}
