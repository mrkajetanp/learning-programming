pub fn rc() {
    basics();
    advanced();
}

fn basics() {
    use std::rc::Rc;

    struct Owner {
        name: String,
    }

    struct Gadget {
        id: i32,
        owner: Rc<Owner>,
    }

    // shared ownership
    let gadget_owner: Rc<Owner> = Rc::new(
        Owner {
            name: String::from("Gadget Man"),
        }
    );

    let gadget1 = Gadget {
        id: 1,
        owner: gadget_owner.clone(),
    };

    let gadget2 = Gadget {
        id: 2,
        owner: gadget_owner.clone(),
    };

    // dispose of our local gadget owner (it's just one Rc)
    drop(gadget_owner);

    // "Gadget Man" still remains allocated because there are Rcs pointing to it
    println!("gadget {} owned by {}", gadget1.id, gadget1.owner.name);
    // Rc<T> automatically dereferences to T
    println!("gadget {} owned by {}", gadget2.id, gadget2.owner.name);
    // both Rcs are destroyed => so is the Gadget Man
}

fn advanced() {
    use std::rc::Rc;
    use std::rc::Weak;
    use std::cell::RefCell;

    struct Owner {
        name: String,
        gadgets: RefCell<Vec<Weak<Gadget>>>,
    }

    struct Gadget {
        id: i32,
        owner: Rc<Owner>,
    }

    let gadget_owner: Rc<Owner> = Rc::new(
        Owner {
            name: String::from("Gadget Man"),
            gadgets: RefCell::new(vec![]),
        }
    );

    let gadget1 = Rc::new(
        Gadget {
            id: 1,
            owner: gadget_owner.clone(),
        }
    );

    let gadget2 = Rc::new(
        Gadget {
            id: 2,
            owner: gadget_owner.clone(),
        }
    );

    // RefCell dynamic borrow
    {
        let mut gadgets = gadget_owner.gadgets.borrow_mut();
        gadgets.push(Rc::downgrade(&gadget1));
        gadgets.push(Rc::downgrade(&gadget2));
    }

    for gadget_weak in gadget_owner.gadgets.borrow().iter() {
        // gadget_weak is a Weak<Gadget>, upgrade => Option<Rc<Gadget>>
        let gadget = gadget_weak.upgrade().unwrap();
        println!("gadget {} owned by {}", gadget.id, gadget.owner.name);
    }
}
