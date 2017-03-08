pub fn ifs() {
    println!("***Ifs***");
    ifs_one();
    println!("");
}

fn ifs_one() {
    let x = 5;

    if x == 5 {
        println!("x is five!");
    }
    else if x == 6 {
        println!("x is six!");
    }
    else {
        println!("x is neither five nor six");

    }

    let y = if x == 5 { 2 } else { 8 };
    println!("y is: {}", y);
}
