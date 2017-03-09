pub fn references_and_borrowing() {
    println!("***References and Borrowing***");

    let v1 = vec![1, 2, 3, 4];
    let v2 = vec![3, 4, 5, 6];

    let res = foo_one(&v1, &v2); // borrowing a reference
    println!("result is {}", res);
    println!("v1[0] is {}", v1[0]);

    let mut x = 5;
    {
        let y = &mut x;
        *y += 1;
    }
    println!("x is now {}", x);

    let v3 = vec![1, 2, 3];
    for i in &v3 {
        print!("{} ", i);
    }
    println!("");

    let mut v4 = vec![1, 2, 3];
    {
        let v_r = &mut v4;
        v_r.push(8);
    }
    println!("v4[3] is {}", v4[3]);

    println!("");
}

fn foo_one(v1: &Vec<i32>, v2: &Vec<i32>) -> i32 {
    v1[0] + v2[0]
}
