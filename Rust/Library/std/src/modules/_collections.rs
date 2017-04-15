pub fn _collections() {
    println!("*** Collections ***");

    iterators();
    entries();

    binary_heap();

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
