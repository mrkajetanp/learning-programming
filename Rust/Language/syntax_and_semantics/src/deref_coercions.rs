pub fn deref_coercions() {
    println!("***Deref Coercions***");

    derefs();
    deref_and_method_calls();

    println!("");
}

// trait Deref is used to overload *, the dereference operator

use std::ops::Deref;

struct DerefExample<T> {
    value: T,
}

impl<T> Deref for DerefExample<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

fn foo(s: &str) {
    println!("Borrowed str: {}", s);
}

use std::rc::Rc;

fn bar(s: &[i32]) {
    print!("Vec: ");
    for i in s {
        print!("{} ", i);
    }
    println!("");
}

fn derefs() {
    let x = DerefExample { value: 'a' };
    println!("x->value = '{}'", *x);
    assert_eq!('a', *x);

    // String implements Deref<Target=str>
    let owned = "Hello".to_string();

    foo(&owned);

    // counted is now Rc<String>
    let counted = Rc::new(owned);

    // &Rc<String> -> &String -> &str
    foo(&counted);

    // Vec<T> implements Deref<Target=[T]>
    let owned = vec![1, 2, 3];
    bar(&owned);

    // Vectors can deref to a slice
}

struct Foo2;

impl Foo2 {
    fn foo2(&self) {
        println!("foo2 called!");
    }
}

fn deref_and_method_calls() {
    // &&&s coerce to one & so that it can be used as &self argument
    let f = &&Foo2;
    f.foo2();
    (&&&&&&&&&f).foo2();
}
