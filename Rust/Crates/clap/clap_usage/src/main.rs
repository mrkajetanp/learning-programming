
extern crate clap;
use clap::{Arg, App, SubCommand};

fn main() {
    let matches = App::new("Test CLI Program")
        .version("1.0")
        .author("Cajetan Puchalski <cajetan.puchalski@gmail.com>")
        .about("Just tests if it works")
        .arg(Arg::with_name("config")
             .short("c")
             .long("config")
             .value_name("FILE")
             .help("Sets a custom config file")
             .takes_value(true))
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(true)
             .index(1))
        .arg(Arg::with_name("v")
             .short("v")
             .multiple(true)
             .help("Sets the level of verbosity"))
        .subcommand(SubCommand::with_name("test")
                    .about("controls testing features")
                    .version("1.3")
                    .author("nevermind the author")
                    .arg(Arg::with_name("debug")
                         .short("d")
                    .help("print debug info verbosely")))
        .get_matches();

    let config = matches.value_of("config").unwrap_or("default.conf");
    println!("config: {}", config);

    println!("input: {}", matches.value_of("INPUT").unwrap());

    match matches.occurrences_of("v") {
        0 => println!("no verbose info"),
        1 => println!("some verbose info"),
        2 => println!("tons of verbose info"),
        3 | _ => println!("don't be crazy"),
    }

    if let Some(matches) = matches.subcommand_matches("test") {
        if matches.is_present("debug") {
            println!("Debug .. ");
        } else {
            println!("Normal..");
        }
    }
}
