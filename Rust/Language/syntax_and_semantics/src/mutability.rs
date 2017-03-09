pub fn mutability() {
    println!("***Mutability***");

    mutability_one();
    field_level_mutability();

    println!("");
}

fn mutability_one() {
    let mut x = 5;
    println!("x is {}", x);
    x = 7;
    println!("x is {}", x);

    let mut y = &mut x;
    println!("y is {}", y);
    *y += 1;
    println!("y is {}", y);

    let (mut x2, y2) = (5, 8);
    println!("x2 = {}, y2 = {}", x2, y2);
    x2 += 1;
    println!("x2 = {}, y2 = {}", x2, y2);
}

struct Point {
    x: i32,
    y: i32,
    // mut y: i32, // not allowed
}

use std::cell::Cell;

struct PointCell {
    x: i32,
    y: Cell<i32>,
}

fn field_level_mutability() {
    let mut a = Point { x: 5, y: 6 };
    a.x = 10;
    let b = Point { x: 4, y: 7 };
    // b.x = 10; // it's immutable!
    println!("b.x = {}, b.y = {}", b.x, b.y);

    let point_c = PointCell { x: 5, y: Cell::new(6) };
    point_c.y.set(7);
    print!("point_c.x = {}, ", point_c.x);
    println!("y: {:?}", point_c.y);
}
