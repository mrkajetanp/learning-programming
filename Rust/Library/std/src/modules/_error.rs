pub fn error() {
    example();
    trait_methods();
}

fn example() {
    use std::fmt::Display;

    trait Error: Display {
        fn description(&self) -> &str;

        fn cause(&self) -> Option<&Error> { None }
    }
}

fn trait_methods() {
    use std::error::Error;

    match "xc".parse::<u32>() {
        Err(e) => {
            println!("Error: {:?}", e.description());
            println!("Cause: {:?}", e.cause());
        }

        _ => println!("No error"),
    }
}
