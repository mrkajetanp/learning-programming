pub fn _f32() {
    consts();
}

use std;

fn consts() {
    // language-related
    println!("DIGITS: {}", std::f32::DIGITS);
    println!("EPSILON: {}", std::f32::EPSILON);
    println!("INFINITY: {}", std::f32::INFINITY);
    println!("MANTISSA_DIGITS: {}", std::f32::MANTISSA_DIGITS);
    println!("MAX: {}", std::f32::MAX);
    println!("MAX_10_EXP: {}", std::f32::MAX_10_EXP);
    println!("MAX_EXP: {}", std::f32::MAX_EXP);
    println!("MIN: {}", std::f32::MIN);
    println!("MIN_10_EXP: {}", std::f32::MIN_10_EXP);
    println!("MIN_EXP: {}", std::f32::MIN_EXP);
    println!("MIN_POSITIVE: {}", std::f32::MIN_POSITIVE);
    println!("NAN: {}", std::f32::NAN);
    println!("NEG_INFINITY: {}", std::f32::NEG_INFINITY);
    println!("RADIX: {}\n", std::f32::RADIX);

    // mathematical
    println!("E: {}", std::f32::consts::E);
    println!("FRAC_1_PI: {}", std::f32::consts::FRAC_1_PI);
    println!("FRAC_1_SQRT_2: {}", std::f32::consts::FRAC_1_SQRT_2);
    println!("FRAC_2_PI: {}", std::f32::consts::FRAC_2_PI);
    println!("FRAC_2_SQRT_PI: {}", std::f32::consts::FRAC_2_SQRT_PI);
    println!("FRAC_PI_2: {}", std::f32::consts::FRAC_PI_2);
    println!("FRAC_PI_3: {}", std::f32::consts::FRAC_PI_3);
    println!("FRAC_PI_4: {}", std::f32::consts::FRAC_PI_4);
    println!("FRAC_PI_6: {}", std::f32::consts::FRAC_PI_6);
    println!("FRAC_PI_8: {}", std::f32::consts::FRAC_PI_8);
    println!("LN_10: {}", std::f32::consts::LN_10);
    println!("LN_2: {}", std::f32::consts::LN_2);
    println!("LOG10_E: {}", std::f32::consts::LOG10_E);
    println!("LOG2_E: {}", std::f32::consts::LOG2_E);
    println!("PI: {}", std::f32::consts::PI);
    println!("SQRT_2: {}", std::f32::consts::SQRT_2);
}
