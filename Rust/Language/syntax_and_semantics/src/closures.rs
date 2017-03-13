pub fn closures() {
    println!("***Closures***");

    closures_one();
    move_closures();
    closures_as_arguments();
    function_ptrs_and_closures();
    returning_closures();

    println!("");
}

#[allow(unused_mut)]
fn closures_one() {
    let plus_one = |x: i32| x + 1;
    println!("plus_one(1): {}", plus_one(1));
    assert_eq!(2, plus_one(1));

    let plus_two = |x| {
        let mut result: i32 = x;
        result += 1;
        result += 1;
        result
    };

    assert_eq!(4, plus_two(2));

    let mut num = 5;
    let plus_num = |x| x + num;
    // closure borrowed num

    // can't borrow num mutably until it goes out of scope
    // let y = &mut num;

    assert_eq!(10, plus_num(5));
}

#[allow(unused_variables)]
fn move_closures() {
    let num = 5;

    // rather than taking a (mutable) borrow, we took ownership of a copy
    let owns_num = move |x: i32| x + num;
}

fn call_with_one<F>(closure: F) -> i32 where F: Fn(i32) -> i32 {
    closure(1)
}

fn call_with_one_dyn(closure: &Fn(i32) -> i32) -> i32 {
    closure(1)
}

fn closures_as_arguments() {
    let ans = call_with_one(|x| x + 2);
    assert_eq!(3, ans);

    let ans = call_with_one_dyn(&|x| x + 4);
    assert_eq!(5, ans);
}

fn add_one(i: i32) -> i32 {
    i + 1
}

fn function_ptrs_and_closures() {
    let f = add_one;
    let ans = call_with_one_dyn(&f);
    assert_eq!(2, ans);
}

fn factory() -> Box<Fn(i32) -> i32> {
    let num = 5;
    Box::new(move |x| x + num)
}

fn returning_closures() {
    let f = factory(); // f is move |x| x + num
    let ans = f(1);
    assert_eq!(6, ans);
}
