
pub fn variable_bindings() {
    println!("***Variable bindings***");
    bindings_and_patterns();
    mutability();
    println!("");

}

fn bindings_and_patterns() {
    let x = 5;
    println!("x = {}", x);

    let (x, y) = (1, 2);
    println!("x = {}, y = {}", x, y);

    let x: i32 = 7;
    println!("x = {}", x);

    println!("");
}

fn mutability() {
    let mut x = 5;
    println!("x = {}", x);

    x = 10;
    println!("x = {}", x);
}
