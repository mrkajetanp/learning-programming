pub fn slice() {
    basics();
    iteration();
}

fn basics() {
    // slicing a Vec
    let vec = vec![1, 2, 3];
    let int_slice = &vec[..];

    // coercing an array to a slice
    let str_slice: &[&str] = &["one", "two", "three"];

    let x = &mut [1, 2, 3];
    x[1] = 7;
    assert_eq!(&[1, 7, 3], x);
}

fn iteration() {
    let numbers = &[0, 1, 2];

    for n in numbers {
        println!("{} is a number", n);
    }

    let mut scores = [7, 8, 9];
    for score in &mut scores[..] {
        *score += 1;
    }

    for n in scores.iter() {
        print!("{} ", n);
    }
    println!("");
}
