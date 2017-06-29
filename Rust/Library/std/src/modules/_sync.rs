pub fn sync() {
    atomic_module();
    mpsc_module();
    arc_struct();
}

fn atomic_module() {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    let spinlock = Arc::new(AtomicUsize::new(1));

    let spinlock_clone = spinlock.clone();
    let thread = thread::spawn(move || {
        spinlock_clone.store(0, Ordering::SeqCst);
    });

    // wait for the other thread to release the lock
    while spinlock.load(Ordering::SeqCst) != 0 { }

    if let Err(panic) = thread.join() {
        println!("Thread had an error: {:?}", panic);
    }

    use std::sync::atomic::ATOMIC_USIZE_INIT;

    static GLOBAL_THREAD_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

    let old_thread_count = GLOBAL_THREAD_COUNT.fetch_add(1, Ordering::SeqCst);
    println!("live threads: {}", old_thread_count + 1);

    use std::sync::atomic::AtomicBool;

    let atomic_true = AtomicBool::new(true);
    let atomic_false = AtomicBool::new(false);

    let mut some_bool = AtomicBool::new(true);
    assert_eq!(true, *some_bool.get_mut());
    *some_bool.get_mut() = false;
    assert_eq!(false, some_bool.load(Ordering::SeqCst));

    let some_bool = AtomicBool::new(true);
    assert_eq!(true, some_bool.into_inner());

    let some_bool = AtomicBool::new(true);
    assert_eq!(true, some_bool.load(Ordering::Relaxed));

    let some_bool = AtomicBool::new(true);
    some_bool.store(false, Ordering::Relaxed);
    assert_eq!(false, some_bool.load(Ordering::Relaxed));

    let some_bool = AtomicBool::new(true);
    assert_eq!(true, some_bool.swap(false, Ordering::Relaxed));
    assert_eq!(false, some_bool.load(Ordering::Relaxed));
}

fn mpsc_module() {
    use std::thread;
    use std::sync::mpsc::channel;

    let (tx, rx) = channel();
    thread::spawn(move || {
        tx.send(10).unwrap();
    });
    assert_eq!(10, rx.recv().unwrap());

    // shared usage

    // create a shared channel that can be sent along from many threads
    // tx (transmission) is the sending half, and rx (receiving) is the receiving half
    let (tx, rx) = channel();
    for i in 0..10 {
        let tx = tx.clone();
        thread::spawn(move || {
            tx.send(i).unwrap();
        });
    }

    for _ in 0..10 {
        let j = rx.recv().unwrap();
        assert!(0 <= j && j < 10);
    }

    // propagating panics

    // the call to recv() will return an error because the channel has already hung up (or been deallocated)
    let (tx, rx) = channel::<i32>();
    drop(tx);
    assert!(rx.recv().is_err());

    use std::sync::mpsc::sync_channel;

    let (tx, rx) = sync_channel::<i32>(0);
    thread::spawn(move || {
        // this will wait for the parent thread to start receiving
        tx.send(53).unwrap();
    });
    rx.recv().unwrap();
}

fn arc_struct() {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    let five = Arc::new(5);

    for _ in 0..10 {
        let five = five.clone();

        thread::spawn(move || {
            println!("{:?}", five);
        });
    }

    println!("", );

    let val = Arc::new(AtomicUsize::new(5));

    for _ in 0..10 {
        let val = val.clone();

        thread::spawn(move || {
            let v = val.fetch_add(1, Ordering::SeqCst);
            println!("{:?}", v);
        });
    }

    let five = Arc::new(5);

    let weak_five = Arc::downgrade(&five);
}
