extern crate crates_modules;

// use crates_modules::japanese::greetings;
// use crates_modules::japanese::farewells;

// shorter form for multiple imports
use crates_modules::english::{greetings, farewells};
use crates_modules::japanese;

/* Complex Imports */

// extern crate crates_modules as phrases;
// use phrases::japanese::greetings as ja_greetings;
// use phrases::japanese::farewells::*;
// use phrases::english::{self, greetings as en_greetings, farewells as en_farewells};
//  shorthand for
//     use sayings::english;
//     use sayings::english::greetings as en_greetings;
//     use sayings::english::farewells as en_farewells;


fn main() {
    println!("Hello in English: {}", greetings::hello());
    println!("Goodbye in English: {}", farewells::goodbye());

    println!("Hello in Japanese: {}", japanese::hello());
    println!("Hello in Japanese: {}", japanese::goodbye());
}
