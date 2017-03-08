pub fn loops() {
    println!("***Loops***");

    loops_one();

    println!("");

}

fn loops_one() {
    let mut x = 2;

    print!("Loop: ");
    loop {
        print!("{}", x);

        if x == 5 {
            break;
        }

        print!(", ");

        x += 1;
    }
    println!("");

    x = 1;

    print!("While: ");
    while x != 5 {
        print!("{}, ", x);
        x += 1;
    }
    println!("");

    print!("For: ");
    for x in 0..10 {
        print!("{}, ", x);
    }
    println!("");

    println!("For with enumerate:");
    for (i, v) in (2..7).enumerate() {
        println!("i = {}, v = {}", i, v);
    }

    let lines = "hello\nworld".lines();
    for (num, line) in lines.enumerate() {
        println!("num: {}, line: {}", num, line);

    }
    // break and continue work as expected

    // labels
    'outer: for x in 0..4 {
        'inner: for y in 0..4 {
            if x % 2 == 0 { continue 'outer; } // Continues the loop over `x`.
            if y % 2 == 0 { continue 'inner; } // Continues the loop over `y`.
            println!("x: {}, y: {}", x, y);
        }
    }
}
