#![allow(dead_code, unreachable_code, unused_variables)]

pub fn flow_control() {
    println!("***Flow Control***");

    if_else();
    loops();
    whiles();
    for_ranges();
    matching();
    if_let();
    while_let();

    println!("");
}

fn if_else() {
    let n = 5;

    if n < 0 {
        print!("{} is negative", n);
    } else if n > 0 {
        print!("{} is positive", n);
    } else {
        print!("{} is zero!", n);
    }

    let big_n = if n < 10 && n > -10 {
        println!(", and is small number, increase ten-fold");
        10 * n
    } else {
        println!(", and is a big number, reduce by two");
        n / 2
    };

    println!("{} -> {}", n, big_n);
}

fn loops() {

    let mut count = 0_u32;

    println!("Let's count until infinity!");

    // Infinite loop
    loop {
        count += 1;

        if count == 3 {
            println!("three!");
            continue;
        }

        println!("{}", count);

        if count == 5 {
            println!("that's enough!");
            break;
        }
    }

    // nesting and labels

    'outer: loop {
        println!("Entered the outer loop.");

        'inner: loop {
            println!("Entered the inner loop.");

            // this would only break the inner loop
            // break;

            // this breaks the outer loop
            break 'outer;
        }
        unreachable!();
    }
    println!("Exited the outer loop.");
}

fn whiles() {

    // let mut n = 1;
    // while n < 101 {
    //     if n % 15 == 0 {
    //         println!("fizzbuzz");
    //     } else if n % 3 == 0 {
    //         println!("fizz");
    //     } else if n % 5 == 0 {
    //         println!("buzz", );
    //     } else {
    //         println!("{}", n);
    //     }
    //     n += 1;
    // }

}

fn for_ranges() {

    // for n in 1..101 {
    //     if n % 15 == 0 {
    //         println!("fizzbuzz");
    //     } else if n % 3 == 0 {
    //         println!("fizz");
    //     } else if n % 5 == 0 {
    //         println!("buzz");
    //     } else {
    //         println!("{}", n);
    //     }
    // }

}

enum Color {
    Red,
    Blue,
    Green,
    RGB(u32, u32, u32),
}

fn matching() {
    let number = 13;

    match number {
        1 => println!("One!"),
        2 | 3 | 5 | 7 | 11 => println!("This is a prime"),
        13...19 => println!("A teen"),
        _ => println!("ain't special", ),
    }

    let boolean = true;

    let binary = match boolean {
        false => 0,
        true => 1,
    };

    println!("{} -> {}", boolean, binary);

    // destructuring tuples

    let pair = (0, -2);

    match pair {
        (0, y) => println!("first is 0 and y is {:?}", y),
        (x, 0) => println!("first is {:?} and y is 0", x),
        _ => println!("it doesn't matter what they are"),
    }

    // enums

    let color = Color::RGB(112, 17, 40);

    match color {
        Color::Red => println!("The Color is Red!"),
        Color::Green => println!("The Color is Green!"),
        Color::Blue => println!("The Color is Blue!"),
        Color::RGB(r, g, b) => println!("R: {}, G: {}, B: {}", r, g, b),
    }

    // pointers / refs

    let reference = &4;

    match reference {
        &val => println!("Got by destructuring: {:?}", val),
    }

    // to avoid the '&', dereference before matching

    match *reference {
        val => println!("Got by dereferencing: {:?}", val),
    }

    // same as &3
    let ref is_a_ref = 3;

    let value = 5;
    let mut mut_value = 6;

    // use ref keyword to create a reference
    match value {
        ref r => println!("got a reference to a value: {:?}", r),
    }

    match mut_value {
        ref mut m => {
            *m += 10;
            println!("we added 10, mut_value is now {:?}", m);
        }
    }

    // destructuring structs

    struct Foo { x: (u32, u32), y: u32 }

    let foo = Foo { x: (1, 2), y: 3 };
    let Foo { x: (a, b), y } = foo;

    println!("a = {}, b = {}, y = {}", a, b, y);

    let Foo { y: i, x: j } = foo;
    println!("i = {:?}, j = {:?}", i, j);

    let Foo { y, .. } = foo;
    println!("y = {}", y);

    // match guards

    let pair = (2, -2);

    match pair {
        (x, y) if x == y => println!("These are twins"),
        (x, y) if x + y == 0 => println!("animatter!"),
        (x, _) if x % 2 == 1 => println!("The first one is odd.."),
        _ => println!("no corelation.."),
    }

    match age() {
        0 => println!("I'm not born yet?"),
        n @ 1 ... 12 => println!("I'm a child of age {:?}", n),
        n @ 13 ... 19 => println!("I'm a teen of age {:?}", n),
        n => println!("I'm an old person of age {:?}", n),
    }
}

fn if_let() {
    let number = Some(7);
    let letter: Option<i32> = None;
    let emoji: Option<i32> = None;

    if let Some(i) = number {
        println!("Matched {:?}", i);
    }

    if let Some(i) = letter {
        println!("Matched {:?}", i);
    } else {
        println!("Didn't match a number! Let's go with a letter.");
    }

    // altered failing condition
    let i_like_letters = false;

    if let Some(i) = emoji {
        println!("Matched {:?}", i);
    } else if i_like_letters {
        println!("Didn't match a number! Let's go with a letter.");
    } else {
        println!("Maybe go with an emoji instead?");
    }
}

fn while_let() {
    let mut optional = Some(0);

    while let Some(i) = optional {
        if i > 9 {
            println!("Greater than 9, quit!");
            optional = None;
        } else {
            println!("i is {:?}, try again!", i);
            optional = Some(i + 1);
        }
    }
}

fn age() -> u32 {
    15
}
