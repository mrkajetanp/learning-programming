pub fn macros() {
    println!("***Macros***");

    basics();
    advanced();
    common_macros();

    println!("");
}

macro_rules! mvec {
    ( $( $x:expr),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

macro_rules! foo {
    (x => $e:expr) => (println!("mode X: {}", $e));
    (y => $e:expr) => (println!("mode Y: {}", $e));
}

// what the fuck is this?
macro_rules! o_O {
    (
        $(
            $x:expr; [ $( $y:expr ),* ]
        );*
    ) => {
        &[ $($( $x + $y ),*),*]
    }
}

macro_rules! five_times {
    ($x:expr) => (5 * $x);
}

macro_rules! log {
    ($msg:expr) => {{
        // let state: i32 = get_log_state();
        let state: i32 = 3;
        if state > 0 {
            println!("log({}): {}", state, $msg);
        }
    }};
}

macro_rules! foo2 {
    ($v:ident) => (let $v = 3;);
}

macro_rules! foo3 {
    () => (fn z() {
        println!("fn z called!")
    });
}

fn basics() {
    foo!(x => 2);
    foo!(y => 3);

    let a: &[i32] = o_O!(10; [1,2,3]; 20; [4,5,6]);

    for i in a {
        print!("{} ", i);
    }
    println!("");

    assert_eq!(a, [11, 12, 13, 24, 25, 26]);
    // Rust macros, unlike those in C, are hygienic
    assert_eq!(25, five_times!(2 + 3));

    let state: &str = "splines";
    log!(state);

    foo2!(x);
    println!("x: {}", x);

    foo3!();
    z();
}

// recursive macro
#[allow(unused_must_use)]
macro_rules! write_html {
    ($w:expr, ) => (());

    ($w:expr, $e:tt) => (write!($w, "{}", $e));

    ($w:expr, $tag:ident [ $($inner:tt)* ] $($rest:tt)*) => {{
        write!($w, "<{}>", stringify!($tag));
        write_html!($w, $($inner)*);
        write!($w, "</{}>", stringify!($tag));
        write_html!($w, $($rest)*);
    }};
}

#[allow(unused_must_use)]
fn advanced() {
    use std::fmt::Write;

    let mut out = String::new();

    write_html!(&mut out,
                html[
                    head[title["Macros guide"]]
                    body[h1["Macros are very cool!"]]
                ]);

    println!("HTML out: {}", out);
}

#[allow(dead_code)]
fn increment(x: u32) -> u32 {
    x + 1
}

// exporting macros
#[macro_export]
macro_rules! inc {
    ($x:expr) => ( $crate::increment($x) )
}

#[allow(unused_variables, dead_code)]
fn common_macros() {
    // panic!("oh no!"); // causes the thread to panic

    let v1 = vec![1, 2, 3];
    let v2 = vec![0; 10];

    assert!(true);
    assert_eq!(5, 3+2);

    // not really
    // assert!(5 < 3);
    // assert_eq!(5, 3);

    use std;
    use std::fs::File;

    fn foo() -> std::io::Result<()> {
        // will return T if ok, and Err(E) if not okay
        let f = try!(File::create("foo.txt"));
        Ok(())
    }

    // println!("foo(): {:?}", foo());

    fn unimp_test() {
        unimplemented!();
    }

    if false {
        unreachable!();
    }

    let x: Option<i32> = None;

    match x {
        Some(_) => unreachable!(),
        None => println!("I know x is None!"),
    }

    // will panic because unimp_test is not yet implemented
    // unimp_test();
}
