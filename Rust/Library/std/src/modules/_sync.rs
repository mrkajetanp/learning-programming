pub fn sync() {
    atomic_module();
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
