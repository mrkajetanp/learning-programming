pub fn env() {
    consts_module();
    functions();
}

use std::env;

fn consts_module() {
    println!("ARCH: {}", env::consts::ARCH);
    println!("DLL_EXTENSION: {}", env::consts::DLL_EXTENSION);
    println!("DLL_PREFIX: {}", env::consts::DLL_PREFIX);
    println!("DLL_SUFFIX: {}", env::consts::DLL_SUFFIX);
    println!("EXE_EXTENSION: {}", env::consts::EXE_EXTENSION);
    println!("EXE_SUFFIX: {}", env::consts::EXE_SUFFIX);
    println!("FAMILY: {}", env::consts::FAMILY);
    println!("OS: {}", env::consts::OS);
}

fn functions() {
    for i in env::args() {
        println!("{}", i);
    }

    for i in env::args_os() {
        println!("{:?}", i);
    }

    println!("Current dir is: {}", env::current_dir().unwrap().display());
    println!("{:?}", env::current_exe());

    println!("home dir: {:?}", env::home_dir());

    use std::path::PathBuf;

    if let Some(path) = env::var_os("PATH") {
        let mut paths = env::split_paths(&path).collect::<Vec<_>>();
        paths.push(PathBuf::from("/home/cajetan/bin"));
        let new_path = env::join_paths(paths).unwrap();
        println!("new path is: {:?}", new_path);
    }

    let key = "KEY";
    env::set_var(key, "VALUE");
    assert_eq!(env::var(key), Ok("VALUE".to_string()));

    env::remove_var(key);
    // there is var_os too
    assert!(env::var(key).is_err());

    use std::path::Path;

    let root = Path::new("/");
    // assert!(env::set_current_dir(&root).is_ok());
    println!("Successfully changed working directory to {}!", root.display());

    use std::fs::File;

    let mut dir = env::temp_dir();
    dir.push("foo.txt");

    let f = File::create(dir);

    // all env variables of the current process
    for (key, value) in env::vars() {
        println!("{}: {}", key, value);
    }
    // vars_os too
}
