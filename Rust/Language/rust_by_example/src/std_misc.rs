pub fn std_misc() {
    println!("***STD Misc***");

    threads();
    channels();
    paths();
    // file_io();
    // child_processes();
    // filesystem_operations();
    program_arguments();

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

fn child_processes() {
    {
        use std::process::Command;

        let output = Command::new("rustc")
            .arg("--version")
            .output().unwrap_or_else(|e| {
                panic!("Failed to execute a process: {}", e);
            });

        if output.status.success() {
            let s = String::from_utf8_lossy(&output.stdout);

            print!("rustc succeded and stdout was:\n{}", s);
        } else {
            let s = String::from_utf8_lossy(&output.stderr);

            print!("rustc failed and stderr was:\n{}", s);
        }
    }

    /* pipes */
    {
        use std::error::Error;
        use std::io::prelude::*;
        use std::process::{Command, Stdio};

        static PANGRAM: &'static str =
            "the quick brown fox jumped over the lazy dog\n";

        // spawn the wc command
        let process = match Command::new("wc")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn() {
                Err(why) => panic!("Couldn't spawn wc: {}", why.description()),
                Ok(process) => process,
            };

        // write a string to the stdin of wc
        // stdin is Option<ChildStdin>, we can directly unwrap it here
        match process.stdin.unwrap().write_all(PANGRAM.as_bytes()) {
            Err(why) => panic!("couldn't write to wc stdin: {}",
                               why.description()),
            Ok(_) => println!("sent pangram to wc"),
        }

        // stdin does not live after the above calls
        // it's dropped and the pipe is closed
        // otherwise wc wouldn't start processing the input we sent

        // stdout is Option<ChildStdout>
        let mut s = String::new();
        match process.stdout.unwrap().read_to_string(&mut s) {
            Err(why) => panic!("couldn't read wc stdout: {}",
                               why.description()),
            Ok(_) => print!("wc responded with:\n{}", s),
        }

    }

    /* wait */
    {
        use std::process::Command;

        let mut child = Command::new("sleep").arg("1").spawn().unwrap();
        let _result = child.wait().unwrap();

        println!("reached the end!");
    }
}

fn filesystem_operations() {
    use std::fs;
    use std::fs::{File, OpenOptions};
    use std::io;
    use std::io::prelude::*;
    use std::os::unix;
    use std::path::Path;

    // a simple implementation of 'cat path'
    fn cat(path: &Path) -> io::Result<String> {
        let mut f = try!(File::open(path));
        let mut s = String::new();
        match f.read_to_string(&mut s) {
            Ok(_) => Ok(s),
            Err(e) => Err(e),
        }
    }

    // a simple implementation of 'echo s > path'
    fn echo(s: &str, path: &Path) -> io::Result<()> {
        let mut f = try!(File::create(path));

        f.write_all(s.as_bytes())
    }

    // a simple implementation of 'touch path' (ignores existing files)
    fn touch(path: &Path) -> io::Result<()> {
        match OpenOptions::new().create(true).write(true).open(path) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }

    println!("mkdir a");
    // create a directory, returns io::Result<()>
    match fs::create_dir("a") {
        Err(e) => println!("! {:?}", e.kind()),
        Ok(_) => {},
    }

    println!("'echo hello > a/b.txt'");
    // the previous match can be simplified with unwrap_or_else
    echo("hello", &Path::new("a/b.txt")).unwrap_or_else(|e| {
        println!("! {:?}", e.kind());
    });

    println!("mkdir -p a/c/d '");
    // recursively create a directory
    fs::create_dir_all("a/c/d").unwrap_or_else(|e| {
        println!("! {:?}", e.kind());
    });

    println!("touch a/c/e.txt");
    touch(&Path::new("a/c/e.txt")).unwrap_or_else(|e| {
        println!("! {:?}", e.kind());
    });

    println!("ln -s ../b.txt a/c/b.txt");
    // create a symlink
    if cfg!(target_family = "unix") {
        unix::fs::symlink("../b.txt", "a/c/b.txt").unwrap_or_else(|e| {
            println!("! {:?}", e.kind());
        });
    }

    println!("cat a/c/b.txt");
    match cat(&Path::new("a/c/b.txt")) {
        Err(why) => println!("! {:?}", why.kind()),
        Ok(s) => println!("> {}", s),
    }

    println!("ls a");
    // read the contents of a directory, returns io::Result<Vec<Path>>
    match fs::read_dir("a") {
        Err(why) => println!("! {:?}", why.kind()),
        Ok(paths) => for path in paths {
            println!("> {:?}", path.unwrap().path());
        },
    }

    println!("rm a/c/e.txt");
    fs::remove_file("a/c/e.txt").unwrap_or_else(|e| {
        println!("! {:?}", e.kind());
    });

    println!("rmdir a/c/d");
    // remove an empty directory
    fs::remove_dir("a/c/d").unwrap_or_else(|e| {
        println!("! {:?}", e.kind());
    });
}

fn program_arguments() {
    /* basics */
    {
        use std::env;

        let args: Vec<String> = env::args().collect();

        // the first argument is the path
        println!("My path is {}", args[0]);

        // the rest of the arguments are passed command line parameters
        println!("I got {:?} arguments: {:?}", args.len()-1, &args[1..]);
    }

    /* argument parsing */
    {
        use std::env;

        fn increase(number: i32) {
            println!("{}", number+1);
        }

        fn decrease(number: i32) {
            println!("{}", number-1);
        }

        fn help() {
            println!("usage:
match_args <string>
     Check whether given string is the answer.
match_args {{increase|decrease}} <integer>
     Increase or decrease given integer by one");
        }

        let args: Vec<String> = env::args().collect();

        match args.len() {
            // no arguments
            1 => println!("my name is match_args, pass some arguments!"),
            // one argument
            2 => {
                match args[1].parse() {
                    Ok(42) => println!("This is the answer!"),
                    _ => println!("This is not the answer!"),
                }
            },
            // one command and one argument passed
            3 => {
                let cmd = &args[1];
                let num = &args[2];
                // parse the number
                let number: i32 = match num.parse() {
                    Ok(n) => n,
                    Err(_) => {
                        println!("error: second argument is not an integer!");
                        help();
                        return;
                    },
                };
                // parse the command
                match &cmd[..] {
                    "increase" => increase(number),
                    "decrease" => decrease(number),
                    _ => {
                        println!("error: invalid command");
                        help();
                    },
                }
            },
            // all other cases
            _ => {
                // show a help message
                help();
            }
        }
    }
}
