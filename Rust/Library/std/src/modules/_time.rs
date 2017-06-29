pub fn time() {
    basics();
    duration_struct();
    instant_struct();
    system_time_struct();
}

fn basics() {
    use std::time::Duration;

    let five_seconds = Duration::new(5, 0);

    assert_eq!(Duration::new(5, 0), Duration::from_secs(5));
}

fn duration_struct() {
    use std::time::Duration;

    let five_seconds = Duration::new(5, 0);
    let five_seconds_and_five_nanos = five_seconds + Duration::new(0, 5);

    assert_eq!(5, five_seconds_and_five_nanos.as_secs());
    assert_eq!(5, five_seconds_and_five_nanos.subsec_nanos());

    let ten_millis = Duration::from_millis(10);
}

fn instant_struct() {
    use std::time::{Duration, Instant};
    use std::thread::sleep;

    let now = Instant::now();

    // we sleep for 2 seconds
    sleep(Duration::new(2, 0));
    println!("{}", now.elapsed().as_secs());
}

fn system_time_struct() {
    use std::time::{Duration, SystemTime};
    use std::thread::sleep;

    let now = SystemTime::now();

    sleep(Duration::new(2, 0));
    match now.elapsed() {
        Ok(elapsed) => {
            println!("{}", elapsed.as_secs());
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
