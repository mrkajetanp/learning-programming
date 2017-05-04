pub fn result() {
    fn_returning_result();
    convenience_methods();
    the_question_syntax();
}

fn fn_returning_result() {
    #[derive(Debug)]
    enum Version { Version1, Version2 }

    fn parse_version(header: &[u8]) -> Result<Version, &'static str> {
        match header.get(0) {
            None => Err("invalid header length"),
            Some(&1) => Ok(Version::Version1),
            Some(&2) => Ok(Version::Version2),
            Some(_) => Err("invalid version"),
        }
    }

    let version = parse_version(&[1, 2, 3, 4]);
    match version {
        Ok(v) => println!("working with version: {:?}", v),
        Err(e) => println!("error parsing header: {:?}", e),
    }
}

fn convenience_methods() {
    let good_result: Result<i32, i32> = Ok(10);
    let bad_result: Result<i32, i32> = Err(10);

    assert!(good_result.is_ok() && !good_result.is_err());
    assert!(bad_result.is_err() && !bad_result.is_ok());

    // consumes map and produces another one
    let good_result: Result<i32, i32> = good_result.map(|i| i + 1);
    let bad_result: Result<i32, i32> = bad_result.map(|i| i - 1);

    // and_then continues the computation
    let good_result: Result<bool, i32> = good_result.and_then(|i| Ok(i == 11));

    // or_else handles the error
    let bad_result: Result<i32, i32> = bad_result.or_else(|i| Ok(i + 20));

    // consume the result and return the contents with unwrap
    let final_awesome_result = good_result.unwrap();
}

fn the_question_syntax() {
    use std::fs::File;
    use std::io::prelude::*;
    use std::io;

    struct Info {
        name: String,
        age: i32,
        rating: i32,
    }

    fn write_info(info: &Info) -> io::Result<()> {
        let mut file = File::create("my_best_friends.txt")?;
        // early return on error
        file.write_all(format!("name: {}\n", info.name).as_bytes())?;
        file.write_all(format!("age: {}\n", info.age).as_bytes())?;
        file.write_all(format!("rating: {}\n", info.rating).as_bytes())?;
        Ok(())
    }
}
