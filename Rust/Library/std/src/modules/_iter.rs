pub fn _iter() {
    implementing_iterator();
    infinity();
    adaptors();
}

fn implementing_iterator() {
    struct Counter {
        count: usize,
    }

    impl Counter {
        fn new() -> Counter {
            Counter { count: 0 }
        }
    }

    impl Iterator for Counter {
        // counting with usize
        type Item = usize;

        // next() is the only required method
        fn next(&mut self) -> Option<usize> {
            self.count += 1;

            if self.count < 6 {
                Some(self.count)
            } else {
                None
            }
        }
    }

    let mut counter = Counter::new();

    print!("{} ", counter.next().unwrap());
    print!("{} ", counter.next().unwrap());
    print!("{} ", counter.next().unwrap());
    print!("{} ", counter.next().unwrap());
    println!("{}", counter.next().unwrap());
}

fn infinity() {
    let numbers = 0..;
    let five_numbers = numbers.take(5);

    for number in five_numbers {
        print!("{} ", number);
    }
    println!("");
}

fn adaptors() {
    let numbers = [1, 2, 3, 4, 5, 6];
    let mut iter = numbers.iter();
    assert_eq!(Some(&6), iter.next_back());
    assert_eq!(Some(&5), iter.next_back());

    let numbers = [1, 2, 3, 4, 5, 6];
    assert_eq!(Some(&2), numbers.iter().rfind(|&&x| x == 2));

    let numbers = [1, 2, 3, 4, 5, 6];
    assert_eq!(6, numbers.iter().count());

    let numbers = [1, 2, 3, 4, 5, 6];
    assert_eq!(Some(&6), numbers.iter().last());

    let numbers = [1, 2, 3, 4, 5, 6];
    assert_eq!(Some(&4), numbers.iter().nth(3));

    let numbers_1 = [1, 2, 3];
    let numbers_2 = [4, 5, 6];
    let iter = numbers_1.iter().chain(numbers_2.iter());
    assert_eq!(Some(&1), numbers.iter().next());
    assert_eq!(Some(&6), numbers.iter().next_back());

    let numbers_1 = [1, 2, 3];
    let numbers_2 = [4, 5, 6];
    let mut iter = numbers_1.iter().zip(numbers_2.iter());
    assert_eq!(Some((&1, &4)), iter.next());

    let a = ["1", "2", "lol"];
    let mut iter = a.iter().filter_map(|s| s.parse().ok());

    assert_eq!(Some(1), iter.next());
    assert_eq!(Some(2), iter.next());
    assert_eq!(None, iter.next());

    // peekable
    // skip_while
    // take_while

    let a = [1, 2, 3];
    let mut iter = a.iter().scan(1, |state, &x| {
        *state *= x;

        Some(*state)
    });

    assert_eq!(Some(1), iter.next());
    assert_eq!(Some(2), iter.next());
    assert_eq!(Some(6), iter.next());
    assert_eq!(None, iter.next());

    let words = ["alpha", "beta", "gamma"];

    let merged: String = words.iter().flat_map(|s| s.chars()).collect();
    assert_eq!("alphabetagamma", merged);

    // fuse
    // inspect
    // by_ref
    // partition
    // fold

    // all
    // any
    // find
    // position
    // rposition
    // max
    // min
    // max_by_key
    // min_by_key
    // min_by
    // rev
    // unzip
    // cloned
    // cycle
    // sum

    println!("5! = {}", (1..6).product::<i32>());

    // extend
}
