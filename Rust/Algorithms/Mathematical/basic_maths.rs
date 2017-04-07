fn get_factors(n: u64) -> Vec<u64> {
    let mut result: Vec<u64> = vec![];

    for i in 1..n+1 {
        if n % i == 0 {
            result.push(i);
        }
    }

    result
}

fn get_factors_func(n: u64) -> Vec<u64> {
    (1..n+1).into_iter().filter(|&i| n % i == 0).collect::<Vec<u64>>()
}

fn main() {
    println!("{:?}", get_factors(15));
    println!("{:?}", get_factors_func(15));
}
