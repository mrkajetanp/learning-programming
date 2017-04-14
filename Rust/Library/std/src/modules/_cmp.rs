pub fn _cmp() {
    println!("*** cmp ***");

    general();
    ordering_enum();
    eq_partial_eq_traits();
    ord_partial_ord_traits();
    min_max_fns();

    println!("");
}

fn general() {
    let x: u32 = 0;
    let y: u32 = 1;

    // equivalent
    assert_eq!(true, x < y);
    assert_eq!(true, x.lt(&y));

    assert_eq!(false, x == y);
    assert_eq!(false, x.eq(&y));
}

fn ordering_enum() {
    use std::cmp::Ordering;

    assert_eq!(Ordering::Less, 1.cmp(&2));
    assert_eq!(Ordering::Equal, 1.cmp(&1));
    assert_eq!(Ordering::Greater, 2.cmp(&1));

    assert_eq!(Ordering::Greater, Ordering::Less.reverse());
    assert_eq!(Ordering::Less, Ordering::Greater.reverse());
    assert_eq!(Ordering::Equal, Ordering::Equal.reverse());

    let mut data: &mut [_] = &mut [2, 10, 5, 8];

    // sort from largest to smallest
    data.sort_by(|a, b| a.cmp(b).reverse());

    assert_eq!(&mut [10, 8, 5, 2], data);

    assert_eq!(Ordering::Less, Ordering::Equal.then(Ordering::Less));

    let x = (1, 2, 7);
    let y = (1, 5, 3);

    let result = x.0.cmp(&y.0).then(x.1.cmp(&y.1)).then(x.2.cmp(&y.2));
    assert_eq!(Ordering::Less, result);

    assert_eq!(Ordering::Less, Ordering::Equal.then_with(|| Ordering::Less));
}

fn eq_partial_eq_traits() {
    // implementing

    enum BookFormat {
        Paperback,
        Hardback,
        Ebook,
    }

    struct Book {
        isbn: i32,
        format: BookFormat,
    }

    impl PartialEq for Book {
        fn eq(&self, other: &Book) -> bool {
            self.isbn == other.isbn
        }
    }

    impl Eq for Book { }

    let b1 = Book { isbn: 3, format: BookFormat::Paperback };
    let b2 = Book { isbn: 3, format: BookFormat::Hardback };
    let b3 = Book { isbn: 10, format: BookFormat::Paperback };

    assert!(b1 == b2);
    assert!(b1 != b3);
}

fn ord_partial_ord_traits() {
    use std::cmp::Ordering;

    #[derive(Eq)]
    struct Person {
        id: u32,
        name: String,
        height: u32,
    }

    impl Ord for Person {
        fn cmp(&self, other: &Person) -> Ordering {
            self.height.cmp(&other.height)
        }
    }

    impl PartialOrd for Person {
        fn partial_cmp(&self, other: &Person) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl PartialEq for Person {
        fn eq(&self, other: &Person) -> bool {
            self.height == other.height
        }
    }
}

fn min_max_fns() {
    use std::cmp;

    assert_eq!(2, cmp::max(1, 2));
    assert_eq!(1, cmp::min(1, 2));
}
