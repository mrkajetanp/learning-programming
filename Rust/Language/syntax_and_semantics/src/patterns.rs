pub fn patterns() {
    println!("***Patterns***");

    patterns_one();
    destructuring();
    ignoring_bindings();
    ref_and_mut();
    ranges();
    bindings();
    guards();
    // mix_and_match();

    println!("");
}

fn patterns_one() {
    let x = 2;

    // binding for a value in any case
    match x {
        y => println!("x: {}, y: {}", x, y),
    }

    match x {
        1 | 2 => println!("one or two"),
        3 => println!("three"),
        _ => println!("anything", ),
    }
}

struct Point {
    x: i32,
    y: i32,
}

fn destructuring() {
    let origin = Point { x: 1, y: 2 };

    match origin {
        Point { x, y } => println!("({},{})", x, y),
    }

    // with a different name
    match origin {
        Point { x: x1, y: y1 } => println!("({},{})", x1, y1),
    }

    // we don't have to name all the values
    match origin {
        Point { x, .. } => println!("x is {}", x),
    }
}

enum OptionalTuple {
    Value(i32, i32, i32),
    // Missing,
}

fn ignoring_bindings() {
    // match some_value {
        // Ok(value) => println!("got a value: {}", value),
        // Err(_) => println!("an error occurred"),
    // }

    let tuple: (u32, String) = (5, String::from("five"));

    // tuple is (partially) moved because string is moved
    let (x, _s) = tuple;
    assert_eq!(5, x);

    let tuple = (5, String::from("five"));

    println!("Tuple is: {:?}", tuple);
    assert_eq!(5, x);

    // String will be dropped immediately because _ does not bound
    let _ = String::from("  hello   ").trim();
    let x = OptionalTuple::Value(5, -2, 3);

    match x {
        OptionalTuple::Value(..) => println!("Got a tuple!"),
        // OptionalTuple::Missing => println!("No such luck."),
    }
}

fn ref_and_mut() {
    let x = 5;

    match x {
        ref r => println!("Got a reference to {}", r),
        // ref mut mr => println!("Got a reference to {}", mr),
    }
}

fn ranges() {
    let x = 3;

    match x {
        1 ... 5 => println!("one through five"),
        _ => println!("anything"),
    }

    let x = 'h';

    match x {
        'a' ... 'j' => println!("early letter"),
        'k' ... 'z' => println!("late letter"),
        _ => println!("something else"),
    }
}

#[derive(Debug)]
struct Person {
    name: Option<String>,
}

fn bindings() {
    let x = 2;

    match x {
        e @ 1 ... 5 => println!("range*2 = {}", e*2),
        _ => println!("anything"),
    }

    let name = "Steve".to_string();
    let x: Option<Person> = Some(Person { name: Some(name) });

    match x {
        Some(Person { name: ref a @ Some(_), .. }) => println!("{:?}", a),
        _ => {}
    }

    let y = 5;
    match y {
        e @ 1 ... 5 | e @ 8 ... 10 => println!("got a range element: {}", e),
        _ => println!("anything"),
    }
}

enum OptionalInt {
    Value(i32),
    Missing,
}

fn guards() {
    let x = OptionalInt::Value(5);
    let y = OptionalInt::Missing;

    match x {
        OptionalInt::Value(i) if i > 5 => println!("Got an int bigger than five!"),
        OptionalInt::Value(..) => println!("Got an int!"),
        OptionalInt::Missing => println!("No such luck."),
    }

    match y {
        OptionalInt::Value(i) if i > 5 => println!("Got an int bigger than five!"),
        OptionalInt::Value(..) => println!("Got an int!"),
        OptionalInt::Missing => println!("No such luck."),
    }

    let x = 4;
    let y = false;

    match x {
        4 | 5 if y => println!("yes"),
        _ => println!("no"),
    }
}

// fn mix_and_match() {
//     match x {
//         Foo { x: Some(ref name), y: Name } => ...
//     }
// }
