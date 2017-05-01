pub fn process() {
    basic();
    advanced();
}

fn basic() {
    use std::process::Command;

    let mut child = Command::new("/bin/cat")
        .arg("mod.rs")
        .spawn()
        .expect("failed to execute child");

    let ecode = child.wait().expect("failed to wait on child");

    assert!(ecode.success());
}

fn advanced() {
    use std::process::{Command, Stdio};
    use std::io::Write;

    let mut child = Command::new("/bin/cat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to execute child");

    {
        let stdin = child.stdin.as_mut().expect("...");
        stdin.write_all(b"test").expect("failed to write to stdin");
    }

    let output = child
        .wait_with_output()
        .expect("");

    assert_eq!(b"test", output.stdout.as_slice());
}
