pub fn _cell() {
    println!("*** Cell ***");

    mut_inside_immut();
    mut_on_logically_immut();

    cell_struct();
    ref_cell_struct();
    ref_struct();
    unsafe_cell_struct();

    println!("");
}

fn mut_inside_immut() {
    use std::collections::HashMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    let shared_map: Rc<RefCell<_>> = Rc::new(RefCell::new(HashMap::new()));

    shared_map.borrow_mut().insert("netherlands", 1);
    shared_map.borrow_mut().insert("japan", 2);
    shared_map.borrow_mut().insert("uk", 3);
}

fn mut_on_logically_immut() {
    use std::cell::RefCell;

    struct Graph {
        edges: Vec<(i32, i32)>,
        span_tree_cache: RefCell<Option<Vec<(i32, i32)>>>,
    }

    impl Graph {
        fn minimum_spanning_tree(&self) -> Vec<(i32, i32)> {
            // scope to contain the dynamic borrow
            {
                // reference inside the cache cell
                let mut cache = self.span_tree_cache.borrow_mut();
                if cache.is_some() {
                    return cache.as_ref().unwrap().clone();
                }

                // let span_tree = self.calc_span_tree();
                let span_tree = vec![(8,8)];
                *cache = Some(span_tree);
            }

            // if previous cache borrow hasn't fallen out of scope
            // recursive borrow would cause a dynamic thread panic
            self.minimum_spanning_tree()
        }
    }
}

fn cell_struct() {
    use std::cell::Cell;

    let c = Cell::new(5);

    let five = c.get();
    assert_eq!(5, five);

    c.set(10);
    assert_eq!(10, c.get());

    let ptr = c.as_ptr();
    unsafe {
        println!("{:?}", *ptr);
    }

    let mut c = Cell::new(8);
    *c.get_mut() += 1;

    assert_eq!(9, c.get());
}

fn ref_cell_struct() {
    use std::cell::RefCell;

    let c = RefCell::new(5);

    // consumes c
    let five = c.into_inner();
    assert_eq!(5, five);

    let c = RefCell::new(5);

    {
        let c_borrow_1 = c.borrow();
        let c_borrow_2 = c.borrow();
    }

    {
        let m = c.borrow_mut();
        assert!(c.try_borrow().is_err());
    }

    {
        let m = c.borrow();
        assert!(c.try_borrow().is_ok());
    }

    *c.borrow_mut() = 7;
    assert_eq!(*c.borrow(), 7);

    {
        let m = c.borrow();
        assert!(c.try_borrow_mut().is_err());
    }

    assert!(c.try_borrow_mut().is_ok());

    let mut c = RefCell::new(4);

    let ptr = c.as_ptr();
    unsafe {
        println!("ptr: {}", *ptr);
    }

    *c.get_mut() += 1;

    unsafe {
        println!("ptr: {}", *ptr);

    }
}

fn ref_struct() {
    use std::cell::{RefCell, Ref};

    let cell = RefCell::new((5, 'b'));

    let b1: Ref<(u32, char)> = cell.borrow();
    let b2: Ref<u32> = Ref::map(b1, |t| &t.0);

    assert_eq!(5, *b2);

    // there is RefMut as well
}

fn unsafe_cell_struct() {
    use std::cell::UnsafeCell;

    let uc = UnsafeCell::new(5);
    // consumes
    let five = unsafe { uc.into_inner() };
    assert_eq!(5, five);

    let uc = UnsafeCell::new(5);
    let five = uc.get();
    unsafe {
        assert_eq!(5, *five);
    }
}
