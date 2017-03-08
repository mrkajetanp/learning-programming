pub fn functions() {
    println!("***Functions***");

    print_number(5);
    print_number(8);
    print_sum(4, 3);
    print_sum(2, 3);
    println!("3 * 2 = {}", multiply_by_two(3));
    println!("5 * 2 = {}", multiply_by_two(5));
    println!("4 + 1 = {}", foo(4));

    // Function pointers
    let f: fn(i32) -> i32 = plus_two;
    let f2 = plus_two; // with type inference

    println!("6 + 2 = {}", f(6));
    println!("8 + 2 = {}", f2(8));

    println!("3 + 2 = {}", plus_two(3));


    // diverges();

    println!("");
}

fn print_number(x: i32) {
    println!("x is: {}", x);
}

fn print_sum(x: i32, y: i32) {
    println!("sum is: {}", x+y);
}

fn multiply_by_two(x: i32) -> i32 {
    x * 2
}

fn foo(x: i32) -> i32 {
    return x+1;
    // Early return
    // Code here never runs
}

// fn diverges() -> ! {
//     panic!("This function never returns!");
// }

fn plus_two(i: i32) -> i32 {
    i + 2
}
