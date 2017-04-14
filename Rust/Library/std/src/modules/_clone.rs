pub fn _clone() {
    println!("*** Clone ***");

    basic();
    clone_trait();

    println!("");
}

fn basic() {
    let s = String::new();

    let copy = s.clone();

    #[derive(Clone)]
    struct Point {
        x: i32,
        y: i32,
    }

    let point_1 = Point { x: 10, y: 15 };

    let point_2 = point_1.clone();
}

fn clone_trait() {

    #[derive(Copy)]
    struct Stats {
        frequencies: [i32; 100],
    }

    impl Clone for Stats {
        fn clone(&self) -> Stats { *self }
    }

    let hello = "hello";
    assert_eq!("hello", hello.clone());

    let mut a = "";
    a.clone_from(&hello);
    assert_eq!("hello", a);

}
