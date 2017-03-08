pub fn primitive_types() {
    println!("***Primitive Types***");

    booleans();
    chars();
    numeric_types();
    arrays();
    slices();
    tuples();

    println!("");

}

fn booleans() {
    let x = true;
    let y: bool = false;

    println!("x: {}", x);
    println!("y: {}", y);

    println!("");
}

fn chars() {
    let x = 'c';
    println!("x: {}", x);

    println!("");
}

fn numeric_types() {
    let x = 42; // x: i32
    let y = 1.1; // y: f64
    println!("x: {}", x);
    println!("y: {}", y);


    println!("");
}

fn arrays() {
    let a = [1, 2, 3]; // a: [i32, 3]
    let mut m = [4, 5, 6]; // m: [i32, 3]

    let b = [0; 20]; // b: [i32, 20]

    println!("a has {} elements", a.len());
    println!("m[1] = {}", m[1]);
    m[1] = 8;
    println!("m[1] = {}", m[1]);

    println!("b[8] = {}", b[8]);

    println!("");
}

fn slices() {
    let a = [0, 1, 2, 3, 4];
    let complete = &a[..]; // slice with all elements in a
    let slice = &a[1..3];
    println!("complete[0] = {}", complete[0]);
    println!("slice[0] = {}", slice[0]);
    println!("complete len = {}", complete.len());
    println!("slice len = {}", slice.len());

    println!("");
}

fn tuples() {
    let mut x = (1, "hello");
    let y: (i32, &str) = (2, "okay");

    println!("{}, {}", x.0, x.1);

    x = y;
    println!("{}, {}", x.0, x.1);

    // destructuring
    let (x1, y1, z1) = (1, 2, 3);
    println!("it is {} {} {}", x1, y1, z1);

    // (0,) // a single element tuple

    println!("");
}



