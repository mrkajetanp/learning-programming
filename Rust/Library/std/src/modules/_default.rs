pub fn default() {
    default_trait();
}

fn default_trait() {

    #[derive(Default, PartialEq, PartialOrd, Debug)]
    struct SomeOptions {
        foo: i32,
        bar: i32,
    }

    let options = SomeOptions::default();
    let options_2: SomeOptions = Default::default();

    assert_eq!(options, options_2);

    let options = SomeOptions { foo: 42, ..Default::default() };

    // implementing Default by hand

    enum Kind {
        A,
        B,
        C,
    }

    impl Default for Kind {
        fn default() -> Kind { Kind::A }
    }


}
