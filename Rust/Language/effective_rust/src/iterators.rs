pub fn iterators() {
    println!("***Iterators***");

    one();
    consumers();
    two();
    adaptors();

    println!("");
}

fn one() {
    // for x in 0...10 { // inclusive range is still experimental
    for x in 0..10 {
        print!("{} ", x);
    }
    println!("");

    let mut range = 0..10;

    loop {
        match range.next() {
            Some(x) => {
                print!("{} ", x);
            },
            None => { break }
        }
    }
    println!("");


    let nums = vec![1, 2, 3, 4];

    // for in loop automatically handles dereferencing with *num because num is &i32
    for num in &nums {
        print!("{} ", num);
    }
    println!("");

    // you can also dereference "by hand"
    for num in &nums {
        print!("{} ", *num);
    }
    println!("");
}

fn consumers() {
    let one_to_twenty = (1..21).collect::<Vec<i32>>();
    // it will infer the type
    let one_to_twenty_inf = (1..21).collect::<Vec<_>>();

    for num in &one_to_twenty {
        print!("{} ", num);
    }
    println!("");

    for num in &one_to_twenty_inf {
        print!("{} ", num);
    }
    println!("");

    let gt_42 = (0..100).find(|x| *x > 42);

    match gt_42 {
        Some(x) => println!("Found a match - {}", x),
        None => println!("No match found."),
    }

    // sums 1, 2, 3, 4
    let sum = (1..5).fold(0, |sum, x| sum + x);
    println!("sum: {}", sum);
}

fn two() {
    let nums = vec![1, 2, 3, 4];

    for num in nums.iter() {
        print!("{} ", num);
    }
    println!("");
}

fn adaptors() {
    let mapped = (1..20).map(|x| x + 1);

    for num in mapped {
        print!("{} ", num);
    }
    println!("");

    // infinite iterator
    for i in (1..).take(5) {
        print!("{} ", i);
    }
    println!("");

    // filters only even numbers
    for i in (1..23).filter(|&x| x % 2 == 0) {
        print!("{} ", i);
    }
    println!("");
}
