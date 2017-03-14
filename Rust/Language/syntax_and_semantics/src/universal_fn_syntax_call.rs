pub fn universal_fn_syntax_call() {
    println!("***Universal Function Syntax Call***");

    one();

    println!("");
}

trait Foo {
    fn f(&self);
}

trait Bar {
    fn f(&self);
}

struct Baz;

impl Foo for Baz {
    fn f(&self) {
        println!("Baz's implementation of foo.");
    }
}

impl Bar for Baz {
    fn f(&self) {
        println!("Baz's implementation of bar.");
    }
}

trait Foo2 {
    fn foo() -> i32;
}

struct Bar2;

impl Bar2 {
    fn foo() -> i32 {
        20
    }
}

impl Foo2 for Bar2 {
    fn foo() -> i32 {
        10
    }
}

fn one() {
    let b = Baz;
    // b.f(); // can't call because of disambiguity

    Foo::f(&b); // same as f.b();
    Bar::f(&b); // same as f.b();

    // angle-bracket form

    assert_eq!(10, <Bar2 as Foo2>::foo());
    assert_eq!(20, Bar2::foo());
}
