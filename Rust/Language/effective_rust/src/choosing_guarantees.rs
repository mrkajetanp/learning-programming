#![allow(unused_variables)]
pub fn choosing_guarantees() {
    println!("***Choosing Guarantees***");

    basic_pointer_types();
    cell_types();
    synchronous_types();

    println!("");
}

fn basic_pointer_types() {
    let x = Box::new(1);
    let y = x;
    // 'x' is no longer accessible (it was moved)

    // Rc<T> - reference counter for shared data
    // Weak<T> - used with Rc<T>, non-owning but also non-borrowed smart pointer
}

use std::cell::Cell;
use std::cell::RefCell;

fn cell_types() {
    // Cells provide interior mutability

    let x = Cell::new(1);
    let y = &x;
    let z = &x;
    x.set(2);
    y.set(3);
    z.set(4);

    println!("{}", x.get());

    // RefCell has runtime overhead

    let x = RefCell::new(vec![1,2,3,4]);
    {
        println!("{:?}", *x.borrow());
    }

    {
        let mut my_ref = x.borrow_mut();
        my_ref.push(1);
    }

    {
        println!("{:?}", *x.borrow());
    }
}

fn synchronous_types() {
    // Arc - AtomicReferenceCount. Can be sent between threads.
    // added cost of atomics

    // Mutex<T> and RwLock<T>
}

