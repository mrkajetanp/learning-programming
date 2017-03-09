pub fn ownership() {
    println!("***Ownership***");

    ownership_one();

    println!("");
}

fn ownership_one() {
    let v = vec![1, 2, 3];
    let v2 = v; // v was moved to v2

    println!("v2[0] is {}", v2[0]);

    let v2 = take_give_back(v2);
    // gave it back so we still can use it..

    take(v2); // moves v2

    // we can't use it anymore
}

fn take(v: Vec<i32>) {
    println!("in a function v[0] is {}", v[0]);
}

fn take_give_back(v: Vec<i32>) -> Vec<i32> {
    println!("in a function v[0] is {}", v[0]);
    v // return (give back) v
}
