pub fn error_handling() {
    println!("***Error Handling***");

    panics();
    options_unwraps();

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

