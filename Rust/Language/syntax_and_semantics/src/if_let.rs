pub fn if_let() {
    println!("***if let***");

    if_let_one();
    while_let();

    println!("");
}

fn foo(x: i32) {
    println!("x: {:?}", x);

}

fn if_let_one() {
    let option: Option<i32> = Some(78);
    // let option: Option<i32> = None;

    if let Some(x) = option {
        foo(x);
    }
    else {
        println!("Didn't work out so well..");
    }
}

fn while_let() {
    let mut v = vec![1, 2, 3, 5, 7, 11];
    while let Some(x) = v.pop() {
        print!("{} ", x);
    }
    println!("");
}
