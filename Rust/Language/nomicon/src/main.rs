#![allow(dead_code)]

fn main() {
    type_conversions();
}

fn type_conversions() {
    struct Foo {
        x: u32,
        y: u16,
    }

    struct Bar {
        a: u32,
        b: u16,
    }

    fn reinterpret(foo: Foo) -> Bar {
        let Foo { x, y } = foo;
        Bar { a: x, b: y }
    }
}
