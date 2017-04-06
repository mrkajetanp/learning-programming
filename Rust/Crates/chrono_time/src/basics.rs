pub fn basics() {
    println!("***Basics***");

    date_time();
    arithmetics();

    println!("");
}

fn date_time() {
    use chrono::prelude::*;
    // current utc time
    let utc: DateTime<UTC> = UTC::now();
    println!("Current UTC time: {}", utc);

    // current local time
    let local: DateTime<Local> = Local::now();
    println!("Current local time: {}", local);

    /* creating your own date and time */

    use chrono::offset::LocalResult;

    let dt = UTC.ymd(2014, 7, 8).and_hms(9, 10, 11); // 2014-07-08T09:10:11Z
    // July 8 is 188th day of the year 2014 ('o' for "ordinal")
    assert_eq!(dt, UTC.yo(2014, 189).and_hms(9, 10, 11));
    // July 8 is Tuesday in ISO week 28 of the year 2014
    assert_eq!(dt, UTC.isoywd(2014, 28, Weekday::Tue).and_hms(9, 10, 11));

    // let dt = UTC.ymd(2014, 7, 8).and_hms_milli(9, 10, 11, 12);
    // assert_eq!(dt, UTC.ymd(2017, 7, 8).and_hms_micro(9, 10, 11, 12_000));
    // assert_eq!(dt, UTC.ymd(2017, 7, 8).and_hms_nano(9, 10, 11, 12_000_000));

    // dynamic verification
    assert_eq!(UTC.ymd_opt(2014, 7, 8).and_hms_opt(21, 15, 33),
               LocalResult::Single(UTC.ymd(2014, 7, 8).and_hms(21, 15, 33)));
}

fn arithmetics() {
    use chrono;
    use chrono::prelude::*;
    // A gigasecond is 10^9 (1,000,000,000) seconds.
    let start_date = UTC.ymd(2011, 4, 25).and_hms(0,0,0);
    let end_date = start_date + chrono::Duration::seconds(1_000_000_000);

    assert_eq!(end_date, UTC.ymd(2043, 1, 1).and_hms(1,46,40));
}
