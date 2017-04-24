use std::io;
use std::io::prelude::*;
use std::io::SeekFrom;
use std::fs::File;

pub fn _io() {
    read_from_file();
    buf_reader_and_buf_writer();
    // stdio();
    iterator_types();
    fns();
}

fn read_from_file() -> Result<(), io::Error> {
    let mut f = File::open("mod.rs")?;
    let mut buffer = [0; 10];

    // skip to the last 10 bytes of the file
    f.seek(SeekFrom::End(-10))?;

    // read up to 10 bytes
    f.read(&mut buffer)?;

    println!("Read bytes: {:?}", buffer);

    Ok(())
}

fn buf_reader_and_buf_writer() -> Result<(), io::Error> {
    use std::io::BufReader;
    use std::io::BufWriter;

    let f = File::open("mod.rs")?;
    let mut reader = BufReader::new(f);
    let mut buffer = String::new();

    // read a line into the buffer
    reader.read_line(&mut buffer)?;

    println!("{}", buffer);

    let if_create = false;

    if if_create {
        let f = File::create("foo.txt")?;

        {
            let mut writer = BufWriter::new(f);

            // write a byte to a buffer
            writer.write(&[42])?;
        } // the buffer is flushed once writer goes out of scope
    }

    Ok(())
}

fn stdio() -> Result<(), io::Error> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    println!("You typed: {:?}", input.trim());

    io::stdout().write(&[42])?;

    Ok(())
}

fn iterator_types() -> io::Result<()> {
    use std::io::BufReader;

    let f = File::open("mod.rs")?;
    let reader = BufReader::new(f);

    for line in reader.lines() {
        println!("{}", line?);
    }

    Ok(())
}

fn fns() -> io::Result<()> {

    // io::copy(&mut io::stdin(), &mut io::stdout())?;

    Ok(())
}
