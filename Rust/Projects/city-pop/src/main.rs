extern crate getopts;
extern crate rustc_serialize;
extern crate csv;

use getopts::Options;
use std::env;
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::io;
use std::process;

// This struct represents the data in each row of the CSV file
// Type based decoding absolves us of a lot of the nitty gritty error handling
// for example parsing strings as integers or floats
#[derive(Debug, RustcDecodable)]
struct Row {
    country: String,
    city: String,
    accent_city: String,
    region: String,

    // the CSV parser will fill in the correct value for us
    population: Option<u64>,
    latitude: Option<f64>,
    longitude: Option<f64>,
}

struct PopulationCount {
    city: String,
    country: String,
    count: u64,
}

// enum with our possible errors
#[derive(Debug)]
enum CliError {
    Io(io::Error),
    Csv(csv::Error),
    NotFound,
}

use std::fmt;

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CliError::Io(ref err) => err.fmt(f),
            CliError::Csv(ref err) => err.fmt(f),
            CliError::NotFound => write!(f, "No matching cities with a population were found."),
        }
    }
}

impl Error for CliError {
    fn description(&self) -> &str {
        match *self {
            CliError::Io(ref err) => err.description(),
            CliError::Csv(ref err) => err.description(),
            CliError::NotFound => "not found",
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            CliError::Io(ref err) => Some(err),
            CliError::Csv(ref err) => Some(err),
            // our custom error currently does not have an underlying cause
            // but we could modify it so that it does
            CliError::NotFound => None,
        }
    }
}

// From implementations for conversions with try!

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> CliError {
        CliError::Io(err)
    }
}

impl From<csv::Error> for CliError {
    fn from(err: csv::Error) -> CliError {
        CliError::Csv(err)
    }
}

fn print_usage(program: &str, opts: Options) {
    println!("{}", opts.usage(&format!("Usage: {} [options] <city>", program)));
}

fn search<P: AsRef<Path>>(file_path: &Option<P>, city: &str) -> Result<Vec<PopulationCount>, CliError> {
    let mut found = vec![];

    let input: Box<io::Read> = match *file_path {
        None => Box::new(io::stdin()),
        Some(ref file_path) => Box::new(try!(File::open(file_path))),
    };

    let mut rdr = csv::Reader::from_reader(input);
    for row in rdr.decode::<Row>() {
        let row = try!(row);
        match row.population {
            None => { } // skip it
            Some(count) => if row.city == city {
                found.push(PopulationCount {
                    city: row.city,
                    country: row.country,
                    count: count,
                });
            },
        }
    }
    if found.is_empty() {
        Err(CliError::NotFound)
    }
    else {
        Ok(found)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = &args[0];

    let mut opts = Options::new();
    opts.optopt("f", "file", "Choose an input file instead of using STDIN.", "NAME");
    opts.optflag("h", "help", "Show this usage message.");
    opts.optflag("q", "quiet", "Silences errors and warnings.");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(e) => { panic!(e.to_string()) }
    };

    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    let data_path = matches.opt_str("f");

    let city = if !matches.free.is_empty() {
        &matches.free[0]
    }
    else {
        print_usage(&program, opts);
        return;
    };

    match search(&data_path, city) {
        Err(CliError::NotFound) if matches.opt_present("q") => process::exit(1),
        Err(err) => panic!("{}", err),
        Ok(pops) => for pop in pops {
            println!("{}, {}: {:?}", pop.city, pop.country, pop.count);
        }
    }
}
