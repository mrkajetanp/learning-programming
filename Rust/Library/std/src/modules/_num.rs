pub fn _num() {
    fp_category_enum();
}

fn fp_category_enum() {
    use std::num::FpCategory;
    use std::f32;

    let num = 12.4_f32;
    let inf = f32::INFINITY;
    let zero = 0_f32;
    let sub: f32 = 1.175492e-38;
    let nan = f32::NAN;

    assert_eq!(num.classify(), FpCategory::Normal);
    assert_eq!(inf.classify(), FpCategory::Infinite);
    assert_eq!(zero.classify(), FpCategory::Zero);
    assert_eq!(nan.classify(), FpCategory::Nan);
    assert_eq!(sub.classify(), FpCategory::Subnormal);
}

fn wrapping_struct() {
    use std;
    use std::num::Wrapping;

    let zero = Wrapping(0_u32);
    let one = Wrapping(1_u32);

    assert_eq!(std::u32::MAX, (zero-one).0);
}
