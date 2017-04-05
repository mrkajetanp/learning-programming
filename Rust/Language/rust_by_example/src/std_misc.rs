pub fn std_misc() {
    println!("***STD Misc***");

    threads();
    channels();
    paths();
    // file_io();

    println!("");
}

fn threads() {
    use std::thread;

    static NTHREADS: i32 = 10;

    // we are currently in the main thread
    // vector for children
    let mut children = vec![];

    for i in 0..NTHREADS {
        // spin up another thread
        children.push(thread::spawn(move || {
            println!("this is a thread number {}", i);
        }));
    }

    for child in children {
        // wait for the thread to finish, returns a result
        let _ = child.join();
    }
}

fn channels() {
    use std::sync::mpsc::{Sender, Receiver};
    use std::sync::mpsc;
    use std::thread;

    static NTHREADS: i32 = 3;

    // channels have two endpoints - Sender<T> and Receiver<T>
    // T is the type of the message to be transferred
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();

    for id in 0..NTHREADS {
        // the sender endpoint can be copied
        let thread_tx = tx.clone();

        // each thread will send its id via the channel
        thread::spawn(move || {
            // the thread takes ownership over thread_tx
            // each thread queues a message in the channel
            thread_tx.send(id).unwrap();

            // sending is a non-blocking operation
            // the thread will continue immediately after sending its message
            println!("thread {} finished", id);
        });
    }

    // collecting all messages
    let mut ids = Vec::with_capacity(NTHREADS as usize);
    for _ in 0..NTHREADS {
        // the recv method picks a message from the channel
        // recv will block the current thread if there are no messages available
        ids.push(rx.recv());
    }

    // show the order in which the messages were sent
    println!("{:?}", ids);
}

fn paths() {
    use std::path::Path;

    // create a Path from an &'static str
    let path = Path::new(".");

    // the display method returns a Showable structure
    let display = path.display();

    // join merges a path with a byte container using the OS specific
    // separator and returns the new path
    let new_path = path.join("a").join("b");

    // convert the path into a string slice
    match new_path.to_str() {
        None => panic!("new path is not a valid UTF-8 sequence"),
        Some(s) => println!("new path is {}", s),
    }
}

fn file_io() {
    /* open */
    {
        use std::error::Error;
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::Path;

        // create a path to the desired file
        let path = Path::new("hello.txt");
        let display = path.display();

        // open the path in read-only mode, returns io::Result<File>
        let mut file = match File::open(&path) {
            // the description method of io::Error returns a string
            // that describes the error
            Err(why) => panic!("couldn't open {}: {}", display,
                               why.description()),
            Ok(file) => file,
        };

        // read the file contents into a string, returns io::Result<usize>
        let mut s = String::new();
        match file.read_to_string(&mut s) {
            Err(why) => panic!("couldn't read {}: {}", display,
                               why.description()),
            Ok(_) => println!("{} contains:\n{}", display, s),
        }

        // file goes out of scope and the "hello.txt" file gets closed
    }

    /* create */
    {
        static LOREM_IPSUM: &'static str =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit,sed du eiusmod tempor incidiunt ut labore et dolore magna aliqua.";

        use std::error::Error;
        use std::io::prelude::*;
        use std::fs::File;
        use std::path::Path;

        let path = Path::new("lorem_ipsum.txt");
        let display = path.display();

        // Open a file in write-only mode, returns io::Result<File>
        let mut file = match File::create(&path) {
            Err(why) => panic!("couldn't create {}: {}",
                               display,
                               why.description()),
            Ok(file) => file,
        };

        // write the LOREM_IPSUM string to file, returns io::Result<()>
        match file.write_all(LOREM_IPSUM.as_bytes()) {
            Err(why) => panic!("couldn't write to {}: {}", display,
                               why.description()),
            Ok(_) => println!("successfully wrote to {}", display),
        }
    }
}
