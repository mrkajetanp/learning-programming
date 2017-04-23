pub fn fmt() {
    format_macro_usage();
    implementing_formatting_traits();
    related_macros();
}

fn format_macro_usage() {
    // basics
    format!("{1} {} {0} {}", 1, 2); // 2 1 1 2
    format!("{argument}", argument = "test"); // test
    format!("{name} {}", 1, name = 2); // 2 1
    format!("{a} {c} {b}", a="a", b='b', c = 3); // a 3 b

    // argument types
    assert_eq!("1.23", format!("{:.*}", 2, 1.234567));
}

fn implementing_formatting_traits() {
    use std::fmt;

    #[derive(Debug)]
    struct Vector2D {
        x: isize,
        y: isize,
    }

    impl fmt::Display for Vector2D {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({}, {})", self.x, self.y)
        }
    }

    // prints the magnitude of a vector
    impl fmt::Binary for Vector2D {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let magnitude = (self.x * self.x + self.y * self.y) as f64;
            let magnitude = magnitude.sqrt();

            let decimals = f.precision().unwrap_or(3);
            let string = format!("{:.*}", decimals, magnitude);
            f.pad_integral(true, "", &string)
        }
    }

    let my_vector = Vector2D { x: 3, y: 4 };
    println!("{}", my_vector);
    println!("{:?}", my_vector);
    println!("{:?}", my_vector);
    println!("{:10.3b}", my_vector);
}

fn related_macros() {
    use std::io::Write;
    let mut v = Vec::new();
    let c = write!(&mut v, "Hello {}!", "world");

    use std::fmt;
    use std::io;

    let mut some_writer = io::stdout();
    let c = write!(&mut some_writer, "{}", format_args!("print with a {}", "macro"));

    fn my_fmt_fn(args: fmt::Arguments) {
        let c = write!(&mut io::stdout(), "{}", args);
    }

    my_fmt_fn(format_args!(", or a {} too", "function"));
}
