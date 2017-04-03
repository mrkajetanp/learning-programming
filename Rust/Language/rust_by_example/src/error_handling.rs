pub fn error_handling() {
    println!("***Error Handling***");

    panics();
    options_unwraps();
    results();
    multiple_error_types();
    defining_error_types();
    other_try();
    boxing_errors();

    println!("");
}

fn panics() {

    fn give_princess(gift: &str) {
        if gift == "snake" {
            panic!("AAAAAAaaaaaaa!");
        }

        println!("I love {}s!!", gift);
    }

    give_princess("teddy bear");
    // give_princess("snake");
}

fn options_unwraps() {
    {
        fn give_commoner(gift: Option<&str>) {
            match gift {
                Some("snake") => println!("I'm throwing that snake in a fire.."),
                Some(inner) => println!("{}? How nice!", inner),
                None => println!("No gift? Oh well."),
            }
        }

        fn give_princess(gift: Option<&str>) {
            let inside = gift.unwrap();
            if inside == "snake" {
                panic!("AAAAaaa!");
            }
            println!("I love {}s!", inside);
        }

        let food = Some("cabbage");
        let snake = Some("snake");
        let void = None;

        give_commoner(food);
        give_commoner(snake);
        give_commoner(void);

        let bird = Some("robin");
        // let nothing = None;

        give_princess(bird);
        // give_princess(nothing);

        /* Combinators - Map */

    }

    {
        #[derive(Debug)]
        enum Food {
            Apple,
            Carrot,
            Potato
        }

        #[derive(Debug)]
        struct Peeled(Food);

        #[derive(Debug)]
        struct Chopped(Food);

        #[derive(Debug)]
        struct Cooked(Food);

        fn peel(food: Option<Food>) -> Option<Peeled> {
            match food {
                Some(food) => Some(Peeled(food)),
                None => None,
            }
        }

        fn chop(peeled: Option<Peeled>) -> Option<Chopped> {
            match peeled {
                Some(Peeled(food)) => Some(Chopped(food)),
                None => None,
            }
        }

        fn cook(chopped: Option<Chopped>) -> Option<Cooked> {
            chopped.map(|Chopped(food)| Cooked(food))
        }

        fn process(food: Option<Food>) -> Option<Cooked> {
            food.map(|f| Peeled(f))
                .map(|Peeled(f)| Chopped(f))
                .map(|Chopped(f)| Cooked(f))
        }

        fn eat(food: Option<Cooked>) {
            match food {
                Some(food) => println!("Mmm. I love it {:?}", food),
                None => println!("Oh no! It wasn't edible."),
            }
        }

        let apple = Some(Food::Apple);
        let carrot = Some(Food::Carrot);
        let potato = None;

        let cooked_apple = cook(chop(peel(apple)));
        let cooked_carrot = cook(chop(peel(carrot)));
        let cooked_potato = process(potato);

        eat(cooked_apple);
        eat(cooked_carrot);
        eat(cooked_potato);
    }

    /* Combinators - and_then */
    {
        #[derive(Debug)]
        enum Food {
            CordonBleu,
            Steak,
            Sushi,
        }

        #[derive(Debug)]
        enum Day {
            Monday,
            Tuesday,
            Wednesday,
        }

        fn have_ingredients(food: Food) -> Option<Food> {
            match food {
                Food::Sushi => None,
                _ => Some(food),
            }
        }

        fn have_recipe(food: Food) -> Option<Food> {
            match food {
                Food::CordonBleu => None,
                _ => Some(food),
            }
        }

        fn cookable_v1(food: Food) -> Option<Food> {
            match have_ingredients(food) {
                None => None,
                Some(food) => match have_recipe(food) {
                    None => None,
                    Some(food) => Some(food),
                },
            }
        }

        fn cookable_v2(food: Food) -> Option<Food> {
            have_ingredients(food).and_then(have_recipe)
        }

        fn eat(food: Food, day: Day) {
            match cookable_v2(food) {
                Some(food) => println!("Yay! On {:?} we get to eat {:?}.", day, food),
                None => println!("Oh no. We don't get to eat on {:?}", day),
            }
        }

        let (cordon_bleu, steak, sushi) = (Food::CordonBleu, Food::Steak, Food::Sushi);

        eat(cordon_bleu, Day::Monday);
        eat(steak, Day::Tuesday);
        eat(sushi, Day::Wednesday);
    }
}

fn results() {
    {
        fn double_number(number_str: &str) -> i32 {
            2 * number_str.parse::<i32>().unwrap()
        }

        let twenty = double_number("10");
        println!("double is {}", twenty);

        // panics!
        // let tt = double_number("t");
        // println!("double is {}", tt);

    }
    /* map for Result */
    {
        use std::num::ParseIntError;

        fn double_number(number_str: &str) -> Result<i32, ParseIntError> {
            match number_str.parse::<i32>() {
                Ok(n) => Ok(2 * n),
                Err(e) => Err(e),
            }
        }

        /* identical as the function above */

        fn double_number_map(number_str: &str) -> Result<i32, ParseIntError> {
            number_str.parse::<i32>().map(|n| 2 * n)
        }

        fn print(result: Result<i32, ParseIntError>) {
            match result {
                Ok(n) => println!("n is {}", n),
                Err(e) => println!("Error: {}", e),
            }
        }

        let twenty = double_number("10");
        print(twenty);

        let tt = double_number_map("t");
        print(tt);
    }

    /* aliases for Result */
    {
        use std::num::ParseIntError;

        // generic alias for a Result with the error type ParseIntError
        type AliasedResult<T> = Result<T, ParseIntError>;

        fn double_number(number_str: &str) -> AliasedResult<i32> {
            number_str.parse::<i32>().map(|n| 2 * n)
        }

        fn print(result: AliasedResult<i32>) {
            match result {
                Ok(n) => println!("n is: {}", n),
                Err(e) => println!("Error: {}", e),
            }
        }

        print(double_number("10"));
        print(double_number("t"));
    }
}

use std;

fn multiple_error_types() {
    type Result<T> = std::result::Result<T, String>;

    fn double_first(vec: Vec<&str>) -> Result<i32> {
        vec.first()
        // convert the option to a Result if there is a value
        // otherwise provide an Err containing a string
            .ok_or("Please use a vector with at least one element.".to_owned())
        .and_then(|s| s.parse::<i32>()
                  // map any error parse yields to string
                  .map_err(|e| e.to_string())
                  // Result<T, String> is the new return type
                  // we can now double the number inside
                  .map(|i| 2 * i))
    }

    fn print(result: Result<i32>) {
        match result {
            Ok(n) => println!("The first doubled is {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }

    let empty = vec![];
    let strings = vec!["tofu", "93", "18"];

    print(double_first(empty));
    print(double_first(strings));

    /* early returns */
    {
        type Result<T> = std::result::Result<T, String>;

        fn double_first(vec: Vec<&str>) -> Result<i32> {
            // Option -> Result if there is a value
            // otherwise - provide Err containing this String
            let first = match vec.first() {
                Some(first) => first,
                None => return Err("Please use a vector with at least one element.".to_owned()),
            };

            // double the number inside if parse works fine
            // otherwise, map any errors that parse yields to String
            match first.parse::<i32>() {
                Ok(i) => Ok(2 * i),
                Err(e) => Err(e.to_string()),
            }
        }

        fn print(result: Result<i32>) {
            match result {
                Ok(n) => println!("The first doubled is: {}", n),
                Err(e) => println!("Error: {}", e),
            }
        }

        let empty = vec![];
        let strings = vec!["tofu", "93", "18"];

        print(double_first(empty));
        print(double_first(strings));
    }

    /* try! macro */
    {
        type Result<T> = std::result::Result<T, String>;

        fn double_first(vec: Vec<&str>) -> Result<i32> {
            let first = try!(vec.first()
            .ok_or("Please use a vector with at least one element.".to_owned()));

            let value = try!(first.parse::<i32>()
            .map_err(|e| e.to_string()));

            Ok(2 * value)
        }

        fn print(result: Result<i32>) {
            match result {
                Ok(n) => println!("The first doubled is {}", n),
                Err(e) => println!("Error: {}", e),
            }
        }

        let empty = vec![];
        let strings = vec!["tofu", "93", "18"];

        print(double_first(empty));
        print(double_first(strings));
    }
}

fn defining_error_types() {
    use std::num::ParseIntError;
    use std::fmt;

    type Result<T> = std::result::Result<T, DoubleError>;

    #[derive(Debug)]
    enum DoubleError {
        EmptyVec,
        Parse(ParseIntError),
    }

    impl fmt::Display for DoubleError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                DoubleError::EmptyVec =>
                    write!(f, "please use a vector with at least one element"),
                DoubleError::Parse(ref e) => e.fmt(f),
            }
        }
    }

    fn double_first(vec: Vec<&str>) -> Result<i32> {
        vec.first()
            // change the error to our new type
            .ok_or(DoubleError::EmptyVec)
            .and_then(|s| s.parse::<i32>()
                      // update to a new error type here also
                      .map_err(DoubleError::Parse)
                      .map(|i| 2 * i))
    }

    fn print(result: Result<i32>) {
        match result {
            Ok(n) => println!("The first doubled is: {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }

    let numbers = vec!["98", "13"];
    let empty = vec![];
    let strings = vec!["tofu", "178"];

    print(double_first(numbers));
    print(double_first(empty));
    print(double_first(strings));
}

fn other_try() {
    use std::num::ParseIntError;
    use std::fmt;

    type Result<T> = std::result::Result<T, DoubleError>;

    #[derive(Debug)]
    enum DoubleError {
        EmptyVec,
        Parse(ParseIntError),
    }

    // conversion ParseIntError -> DoubleError
    // this will be automatically called by try!
    // if there is a need for conversion
    impl From<ParseIntError> for DoubleError {
        fn from(err: ParseIntError) -> DoubleError {
            DoubleError::Parse(err)
        }
    }

    impl fmt::Display for DoubleError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                DoubleError::EmptyVec =>
                    write!(f, "please use a vector with at least one element"),
                DoubleError::Parse(ref e) => e.fmt(f),
            }
        }
    }

    fn double_first(vec: Vec<&str>) -> Result<i32> {
        let first = try!(vec.first().ok_or(DoubleError::EmptyVec));
        let parsed = try!(first.parse::<i32>());

        Ok(2 * parsed)
    }

    fn print(result: Result<i32>) {
        match result {
            Ok(n) => println!("The first doubled is {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }

    let numbers = vec!["94", "18"];
    let empty = vec![];
    let strings = vec!["tofu", "93"];

    print(double_first(numbers));
    print(double_first(empty));
    print(double_first(strings));
}

fn boxing_errors() {
    use std::error;
    use std::fmt;
    use std::num::ParseIntError;

    type Result<T> = std::result::Result<T, Box<error::Error>>;

    #[derive(Debug)]
    enum DoubleError {
        EmptyVec,
        Parse(ParseIntError),
    }

    impl From<ParseIntError> for DoubleError {
        fn from(err: ParseIntError) -> DoubleError {
            DoubleError::Parse(err)
        }
    }

    impl fmt::Display for DoubleError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                DoubleError::EmptyVec =>
                    write!(f, "please use a vector with at least one element"),
                DoubleError::Parse(ref e) => e.fmt(f),
            }
        }
    }

    impl error::Error for DoubleError {
        fn description(&self) -> &str {
            match *self {
                DoubleError::EmptyVec => "empty vectors not allowed",
                DoubleError::Parse(ref e) => e.description(),
            }
        }

        fn cause(&self) -> Option<&error::Error> {
            match *self {
                DoubleError::EmptyVec => None,
                DoubleError::Parse(ref e) => Some(e),
            }
        }
    }

    fn double_first(vec: Vec<&str>) -> Result<i32> {
        let first = try!(vec.first().ok_or(DoubleError::EmptyVec));
        let parsed = try!(first.parse::<i32>());

        Ok(2 * parsed)
    }

    fn print(result: Result<i32>) {
        match result {
            Ok(n) => println!("The first doubled is {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }

    let numbers = vec!["93", "83"];
    let empty = vec![];
    let strings = vec!["tofu", "83"];

    print(double_first(numbers));
    print(double_first(empty));
    print(double_first(strings));
}
