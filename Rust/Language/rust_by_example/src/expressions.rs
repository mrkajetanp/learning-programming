pub fn expressions() {
    println!("***Expressions***");

    basics();

    println!("");
}

fn basics() {
    let x = 5_u32;

    let y = {
        let x_squared = x*x;
        let x_cube = x_squared * x;

        // expression assigned to y
        x_cube + x_squared + x
    };

    let z = {
        // return type is ()
        // it's a statement, not an expression
        2 * x;
    };

    println!("x: {:?}", x);
    println!("y: {:?}", y);
    println!("z: {:?}", z);
}
