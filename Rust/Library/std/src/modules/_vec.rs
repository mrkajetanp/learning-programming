pub fn vec() {
    basics();
    methods();
}

fn basics() {
    let mut vec = Vec::new();
    vec.push(1);
    vec.push(2);

    assert_eq!(2, vec.len());
    assert_eq!(1, vec[0]);

    assert_eq!(Some(2), vec.pop());
    assert_eq!(1, vec.len());

    vec[0] = 7;
    assert_eq!(7, vec[0]);

    vec.extend([1, 2, 3].iter().cloned());

    for x in &vec {
        print!("{} ", x);
    }
    println!("");

    assert_eq!(vec![7, 1, 2, 3], vec);

    let mut vec = vec![1, 2, 3];
    vec.push(4);
    assert_eq!(vec![1, 2, 3, 4], vec);

    let vec = vec![0; 5];
    assert_eq!(vec![0, 0, 0, 0, 0], vec);

    let mut stack = Vec::new();

    stack.push(1);
    stack.push(2);
    stack.push(3);

    while let Some(top) = stack.pop() {
        print!("{} ", top);
    }
    println!("");

    fn read_slice(slice: &[usize]) {
        println!("slice: {:?}", slice);
    }

    let v = vec![0, 1];
    read_slice(&v);
    let x: &[usize] = &v;
}

fn methods() {
    let v = vec![10, 20, 30, 40];
    assert!(v.contains(&30));
    assert!(!v.contains(&35));

    let mut v = vec![-5, 4, 1, -1, 2, -3];
    v.sort();
    println!("sorted: {:?}", v);
}
