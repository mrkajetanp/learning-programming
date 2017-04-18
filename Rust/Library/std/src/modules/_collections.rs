pub fn _collections() {
    println!("*** Collections ***");

    iterators();
    entries();

    binary_heap();
    btree_map();
    btree_set();

    hash_map();
    // hash set looks the same as btree_set..

    println!("");
}

fn iterators() {
    let mut vec = vec![1, 2, 3, 4];

    for x in vec.iter() {
        print!("{} ", x);
    }
    println!("");

    for x in vec.iter_mut() {
        *x += 1;
        print!("{} ", x);
    }
    println!("");

    let vec2 = vec![10, 20, 30, 40];
    vec.extend(vec2);

    use std::collections::VecDeque;
    let buf: VecDeque<_> = vec.into_iter().collect();
}

fn entries() {
    use std::collections::btree_map::BTreeMap;

    let mut count = BTreeMap::new();
    let message = "sea shells by the shore";

    for c in message.chars() {
        *count.entry(c).or_insert(0) += 1;
    }

    assert_eq!(Some(&4), count.get(&'s'));

    for (ch, count) in &count {
        print!("{}: {}, ", ch, count);
    }
    println!("");
}

fn binary_heap() {
    // BinaryHeap struct
    {
        use std::collections::BinaryHeap;

        let mut heap = BinaryHeap::new();

        // nothing there yet
        assert_eq!(None, heap.peek());

        // pushing onto the heap
        heap.push(1);
        heap.push(5);
        heap.push(2);

        // peek what's "on top"
        assert_eq!(Some(&5), heap.peek());

        // number of items in the heap
        assert_eq!(3, heap.len());

        // in "random" order
        for x in &heap {
            print!("{} ", x);
        }
        println!("");


        // popping is in correct order
        assert_eq!(Some(5), heap.pop());
        assert_eq!(Some(2), heap.pop());
        assert_eq!(Some(1), heap.pop());
        assert_eq!(None, heap.pop());

        // clear anything that remains
        heap.clear();

        // not heap is empty
        assert!(heap.is_empty());

        // creating with preallocated capacity
        let mut heap = BinaryHeap::with_capacity(10);
        assert!(heap.capacity() >= 10);
        heap.push(4);

        *heap.peek_mut().unwrap() += 1;
        assert_eq!(Some(&5), heap.peek());

        heap.reserve_exact(8);
        heap.reserve(8);
        heap.shrink_to_fit();

        heap.push(3);
        heap.push(7);
        heap.push(4);

        // println!("Heap into vec: {:?}", heap.into_vec());
        println!("Heap into sorted vec: {:?}", heap.into_sorted_vec());

        let mut heap = BinaryHeap::new();
        heap.push(3);
        heap.push(7);
        heap.push(4);

        for x in heap.drain() {
            print!("{} ", x);
        }
        println!("");

        assert!(heap.is_empty());

        let v = vec![-4, 8, 3, 6, -3];
        let mut a = BinaryHeap::from(v);

        let v = vec![-20, 5, 43];
        let mut b = BinaryHeap::from(v);

        a.append(&mut b);

        println!("{:?}", a.into_sorted_vec());

        assert!(b.is_empty());
    }
}

fn btree_map() {
    // general example
    {
        use std::collections::BTreeMap;

        let mut movie_reviews = BTreeMap::new();

        // review some movies
        movie_reviews.insert("Pulp Fiction", "Masterpiece");
        movie_reviews.insert("The Godfather", "Very enjoyable");
        movie_reviews.insert("The Blues Brothers", "Eye lyked it alot");

        // check for a specific one
        if !movie_reviews.contains_key("Les Misérables") {
            println!("We've got {} reviews, but Les Misérables ain't one.",
                     movie_reviews.len());
        }

        // this one has spelling mistakes
        movie_reviews.remove("The Blues Brothers");

        // look up the values associated with some keys
        let to_find = ["Up!", "The Godfather"];

        for book in &to_find {
            match movie_reviews.get(book) {
                Some(review) => println!("{}: {}", book, review),
                None => println!("{} is unreviewed.", book),
            }
        }

        // iterate over everything
        for (movie, review) in &movie_reviews {
            println!("{}: \"{}\"", movie, review);

        }
    }

    // entry api
    {
        use std::collections::BTreeMap;

        let mut player_stats = BTreeMap::new();

        fn random_stat_buff() -> u8 {
            42
        }

        // insert a key only if it doesn't exist
        player_stats.entry("health").or_insert(100);

        // same, just using a function
        player_stats.entry("defence").or_insert_with(random_stat_buff);

        // update a key guarding against it possibly not being set
        let stat = player_stats.entry("attack").or_insert(100);
        *stat += random_stat_buff();
    }

    // methods on a BTreeMap
    {
        use std::collections::BTreeMap;

        let mut map = BTreeMap::new();

        map.insert(1, "a");

        if let Some(x) = map.get_mut(&1) {
            *x = "b";
        }

        assert_eq!(map.entry(1).key(), &1);

        map.clear();
        assert!(map.is_empty());

        // methods on entry struct
    }
}

fn btree_set() {
    // general example
    {
        use std::collections::BTreeSet;

        let mut books = BTreeSet::new();

        books.insert("A Dance with Dragons");
        books.insert("To Kill a Mockingbird");
        books.insert("The Odyssey");
        books.insert("The Great Gatsby");

        // check for a specific one
        if !books.contains("The Winds of Winter") {
            println!("We have {} books, but The Winds of Winter ain't one.",
                     books.len());
        }

        books.remove("The Odyssey");

        for book in &books {
            println!("{}, ", book);
        }
        println!("");
    }

    // methods
    {
        use std::collections::BTreeSet;

        let mut a = BTreeSet::new();
        a.insert(1);
        a.insert(2);

        let mut b = BTreeSet::new();
        b.insert(2);
        b.insert(3);

        println!("Diff: {:?}", a.difference(&b).cloned().collect::<Vec<i32>>());
        println!("Sym diff: {:?}",
                 a.symmetric_difference(&b).cloned().collect::<Vec<i32>>());
        println!("Inter: {:?}", a.intersection(&b).cloned().collect::<Vec<i32>>());
        println!("Union: {:?}", a.union(&b).cloned().collect::<Vec<i32>>());

        if let Some(x) = a.get(&2) {
            println!("Got {} from set!", x);
        }

        assert!(a.contains(&2), true);

        b.remove(&2);
        assert!(a.is_disjoint(&b));

        a.insert(3);
        assert!(a.is_superset(&b));
        assert!(b.is_subset(&a));

        assert_eq!(Some(3), a.replace(3));
        assert_eq!(None, a.replace(8));

        assert_eq!(Some(3), a.take(&3));
        assert!(!a.contains(&3));

        // append sets
        // split_off

        // sub, bitor, bitxor, bitand operators
        // into_iter (moves out)

        a.clear();
        assert!(a.is_empty());
    }
}

fn hash_map() {
    use std::collections::HashMap;

    let mut book_reviews = HashMap::new();

    book_reviews.insert("Grimm's Fairy Tales", "Masterpiece.");
    book_reviews.insert("A Game of Thrones", "Exceptional.");
    book_reviews.insert("The Adventures of Sherlock Holmes", "Eye lyked it alot.");

    if !book_reviews.contains_key("Les Misérables") {
        println!("We've got {} reviews, but Les Misérables ain't one.",
                 book_reviews.len());
    }

    book_reviews.remove("The Adventures of Sherlock Holmes");

    let to_find = ["A Game of Thrones", "Harry Potter"];
    for book in &to_find {
        match book_reviews.get(book) {
            Some(review) => println!("{}: {}", book, review),
            None => println!("{} is unreviewed.", book),
        }
    }

    for (book, review) in &book_reviews {
        println!("{}: {}", book, review);
    }

    // hash_map implements the entry api

    // to use types in a HashMap, derive Eq, PartialEq & Hash

    let timber_resources: HashMap<&str, i32> =
        [("Norway", 100), ("Denmark", 50), ("Iceland", 10)]
        .iter().cloned().collect();
}
