pub fn _boxed() {
    println!("*** Boxed ***", );

    basics();
    box_struct();

    println!("", );
}

fn basics() {
    let x = Box::new(5);

    // a recursive data structure

    #[derive(Debug)]
    enum List<T> {
        Cons(T, Box<List<T>>),
        Nil,
    }

    let list: List<i32> = List::Cons(1, Box::new(List::Cons(2, Box::new(List::Nil))));
    println!("{:?}", list);
}

fn box_struct() {
    let five = Box::new(5);

    let x = Box::new(5);
    println!("x: {}", x);

    let ptr = Box::into_raw(x);
    unsafe {
        println!("ptr: {}", *ptr);
    }

    let x = unsafe { Box::from_raw(ptr) };
    println!("x: {}", x);

    // downcasting

    use std::any::Any;

    fn print_if_string(value: Box<Any>) {
        if let Ok(string) = value.downcast::<String>() {
            println!("String ({}): {}", string.len(), string);
        }
    }

    let my_string = "Hello world".to_string();
    print_if_string(Box::new(my_string));
    print_if_string(Box::new(0_i8));
}
