pub fn thread() {
    basics();
    thread_struct();
    builder_struct();
    functions();
}

fn basics() {
    use std::thread;

    thread::spawn(move || {
        // some work here
    });

    let child = thread::spawn(move || {
        // some work here
    });

    let res = child.join();

    thread::Builder::new().name("child1".to_string()).spawn(move || {
        println!("Hello, world!");
    });
}

fn thread_struct() {
    use std::thread;

    let handler = thread::Builder::new()
        .name("foo".into())
        .spawn(|| {
            let thread = thread::current();
            println!("thread name: {}", thread.name().unwrap());
        })
        .unwrap();

    handler.join().unwrap();

    let handler = thread::Builder::new()
        .spawn(|| {
            let thread = thread::current();
            thread.unpark();
        })
        .unwrap();

    handler.join().unwrap();

    let builder = thread::Builder::new();

    let handler = builder.spawn(|| {
        assert!(thread::current().name().is_none());
    }).unwrap();

    handler.join().unwrap();

    let builder = thread::Builder::new()
        .name("foo".into());

    let handler = builder.spawn(|| {
        assert_eq!(Some("foo"), thread::current().name());
    }).unwrap();

    handler.join().unwrap();
}

fn builder_struct() {
    use std::thread;

    let builder = thread::Builder::new();

    let handler = builder.spawn(|| {
        // thread code
    }).unwrap();

    handler.join().unwrap();
}

fn functions() {
    use std::{thread, time};

    let ten_millis = time::Duration::from_millis(10);
    let now = time::Instant::now();

    thread::sleep(ten_millis);

    assert!(now.elapsed() >= ten_millis);

    let handle = thread::current();
    assert_eq!("main", handle.name().unwrap());
}
