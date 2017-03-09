pub fn vectors() {
    println!("***Vectors***");

    basics();
    out_of_bounds();
    iterating();

    println!("");
}

fn basics() {
    let v = vec![1, 2, 3, 4, 5]; // v: Vec<i32>
    let v2 = vec![0; 10]; // a vector of 10 0s

    // you must index with usize type
    let i: usize = 5;
    // let i2: i32 = 0;

    println!("v[2] = {}", v[2]);
    println!("v2[i] = {}", v2[i]);
    // println!("v2[i2] = {}", v2[i2]); // won't work

    println!("");

}

fn out_of_bounds() {
    let v = vec![1, 2, 3];

    // println!("will panic -> v[5] = {}", v[5]);

    // out of bounds handling
    let i: usize = 7;

    match v.get(i) {
        Some(x) => println!("Item at {} is {}", i, x),
        None => println!("Sorry, this vector is too short"),
    }

    println!("");
}

fn iterating() {
    let mut v = vec![1, 2, 3];

    for i in &v {
        println!("A reference to {}", i);
    }

    for i in &mut v {
        println!("A mutable reference to {}", i);
    }

    for i in v {
        println!("Take ownership of the vector and its element {}", i);
    }
}
