pub fn scoping() {
    println!("***Scoping***");

    raii();
    ownership_and_moves();
    borrowing();

    println!("");
}

fn raii() {
    fn create_box() {
        // allocate an i32 on the heap
        let _box1 = Box::new(3_i32);

        // box gets destroyed here, memory gets freed
    }

    let _box2 = Box::new(5_i32);

    // a nested scope

    {
        let _box3 = Box::new(4_i32);

        // _box3 is destroyed here
    }

    // creating lots of boxes just for fun
    // they're not bound to anything so they get immediately destroyed
    for _ in 0_u32..1_000 {
        create_box();
    }

    // _box2 destroyed here
}

fn ownership_and_moves() {
    fn destroy_box(c: Box<i32>) {
        println!("Destroying a box that contains {}", c);

        // c is destroyed and memory freed
    }

    // a stack allocated integer
    let x = 5_u32;

    // x gets copied because u32 is a Copy type & it's on the stack
    let y = x;

    // both can be accessed
    println!("x is {}, and y is {}", x, y);

    // a is a ptr to heap allocated i32
    let a = Box::new(5_i32);

    println!("a contains: {}", a);

    // moving a into b
    let b = a;

    println!("b contains: {}", b);

    // a does not own it anymore, it can't be accessed
    // println!("a contains: {}", a);

    destroy_box(b);

    // Heap memory has been freed at this point, can't access b
    // println!("b now contains: {}", b);

    // mutability

    let immutable_box = Box::new(5_u32);

    println!("immutable box contains: {}", immutable_box);

    // mutability error
    // *immutable_box = 4;

    // move it into the mutable binding, changes mutability
    let mut mutable_box = immutable_box;

    println!("mutable_box contains: {}", mutable_box);

    *mutable_box = 4;

    println!("mutable_box now contains: {}", mutable_box);
}

fn borrowing() {
    // function takes ownership of a box and destroyes it
    fn eat_box_i32(boxed_i32: Box<i32>) {
        println!("Eating a box that contains: {}", boxed_i32);
    }

    // this function borrows an i32
    fn borrow_i32(borrowed_i32: &i32) {
        println!("This int is: {}", borrowed_i32);
    }

    // create a boxed i32 and stacked i32
    let boxed_i32 = Box::new(5_i32);
    let stacked_i32 = 6_i32;

    // borrow the contents of the box
    // does not take ownership -> contents can be borrowed again
    borrow_i32(&boxed_i32);
    borrow_i32(&stacked_i32);

    {
        // take a reference to the data inside the box
        let _ref_to_i32: &i32 = &boxed_i32;

        // can't destroy boxed_i32 while the inner value is borrowed
        // eat_box_i32(boxed_i32);
    } // ref_to_i32 goes out of scope - no longer borrowed

    // no more borrows
    // now it can give up its ownership and be destroyed
    eat_box_i32(boxed_i32);

    /* mutability */

    #[derive(Clone, Copy)]
    struct Book {
        author: &'static str,
        title: &'static str,
        year: u32,
    }

    fn borrow_book(book: &Book) {
        println!("I immmutably borrowed {} - {} edition", book.title, book.year);
    }

    fn new_edition(book: &mut Book) {
        book.year = 2017;
        println!("I mutably borrowed {} - {} edition", book.title, book.year);
    }

    let immutabook = Book {
        author: "George R. R. Martin",
        title: "A Game of Thrones",
        year: 1996,
    };

    // mutable copy of immutabook
    let mut mutabook = immutabook;

    borrow_book(&immutabook);

    borrow_book(&mutabook);

    new_edition(&mut mutabook);

    // can't borrow immutable object as mutable
    // new_edition(&mut immutabook);

    /* freezing */

    let mut _mutable_int = 7_i32;

    {
        let _large_int = &_mutable_int;

        // _mutable_int is frozen because it was borrowed!
        // _mutable_int = 50;
    }

    // it's okay here, not borrowed anymore
    _mutable_int = 3;

    struct Point {
        x: i32,
        y: i32,
        z: i32,
    }

    let mut point = Point {
        x: 0,
        y: 0,
        z: 0,
    };

    {
        let borrowed_point = &point;
        let another_borrow = &point;

        // data can be accessed by references and the original owner
        println!("Point is: ({},{},{})", borrowed_point.x, another_borrow.y, point.z);

        // can't borrow mutably - it's already borrowed as immutable
        // let mutable_borrow = &mut point;
    }

    {
        let mutable_borrow = &mut point;

        mutable_borrow.x = 5;
        mutable_borrow.y = 2;
        mutable_borrow.z = 1;

        // can't borrow as immutable - currently borrowed as mutable
        // let y = &point.y;

        // mutable references can be passed as immutable to println

        println!("Point is: ({},{},{})", mutable_borrow.x, mutable_borrow.y, mutable_borrow.z);
    }

    // immutable references to point are allowed again
    let borrowed_point = &point;
    println!("Point is: ({},{},{})", borrowed_point.x, borrowed_point.y, borrowed_point.z);

    /* the ref pattern */
    {
        #[derive(Clone, Copy)]
        struct Point {
            x: i32,
            y: i32,
        }

        let c = 'Q';

        // both have the same effect
        let ref ref_c1 = c;
        let ref_c2 = &c;

        println!("ref_c1 equals ref_c2: {}", *ref_c1 == *ref_c2);

        let point = Point { x: 0, y: 0 };

        let _copy_of_x = {
            // ref_to_x points to the x field of a point
            let Point { x: ref ref_to_x, y: _ } = point;

            // return a copy of the 'x' field
            *ref_to_x
        };

        // a mutable copy of 'point'
        let mut mutable_point = point;

        {
            let Point { x: _, y: ref mut mut_ref_to_y } = mutable_point;

            *mut_ref_to_y = 1;
        }

        println!("point is: ({},{})", point.x, point.y);
        println!("mutable point is: ({},{})", mutable_point.x, mutable_point.y);

        // a mutable tuple that includes a pointer
        let mut mutable_tuple = (Box::new(5_u32), 3_u32);

        {
            let (_, ref mut last) = mutable_tuple;
            *last = 2_u32;
        }

        println!("tuple is: {:?}", mutable_tuple);
    }
}
