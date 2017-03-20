use std::thread;
use std::sync::{Arc, Mutex};
use std::time::Duration;

pub fn concurrency() {
    println!("***Concurrency***");

    one();
    mutex_usage();
    channels_generic();
    channels_specific();
    panics();

    println!("");
}

fn one() {

    let handle = thread::spawn(|| {
        "Hello from another thread!"
    });

    thread::spawn(|| {
        println!("hello from a thread!");
    });

    let x = 1;
    thread::spawn(move || {
        println!("x is {}", x);
    });

    println!("{}", handle.join().unwrap());
}

fn mutex_usage() {
    let data = Arc::new(Mutex::new(vec![1, 2, 3]));

    for i in 0..3 {
        let data = data.clone();
        thread::spawn(move || {
            let mut data = data.lock().unwrap();
            data[0] += i;
        });
    }

    // not a good way to wait for a thread..
    thread::sleep(Duration::from_millis(50));

    println!("{:?}", data);
}

use std::sync::mpsc;

fn channels_generic() {
    let data = Arc::new(Mutex::new(0));

    // 'tx' is the "transmitter" or "sender"
    // 'rx' is the "receiver"
    let (tx, rx) = mpsc::channel();

    for _ in 0..10 {
        let (data, tx) = (data.clone(), tx.clone());

        thread::spawn(move || {
            let mut data = data.lock().unwrap();
            *data += 1;

            tx.send((*data)).unwrap();
        });
    }

    for _ in 0..10 {
        print!("{} ", rx.recv().unwrap());
    }
    println!("");
}

fn channels_specific() {
    let (tx, rx) = mpsc::channel();

    for i in 0..10 {
        let tx = tx.clone();

        thread::spawn(move || {
            // compute a square in another thread
            let ans = i * i;

            // send() back the answer using a channel
            tx.send(ans).unwrap();
        });
    }

    for _ in 0..10 {
        print!("{} ", rx.recv().unwrap());
    }
    println!("");
}

fn panics() {
    let handle = thread::spawn(move || {
        panic!("oops!");
    });

    let result = handle.join();
    assert!(result.is_err());

    println!("Thread panicked but program still runs..");
}
