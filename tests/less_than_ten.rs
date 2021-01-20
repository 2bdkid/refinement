use refinement::{Predicate, Refinement};

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
struct LessThanTenPredicate;

impl Predicate<i32> for LessThanTenPredicate {
    fn test(x: &i32) -> bool {
        *x < 10
    }
}

type LessThanTen = Refinement<i32, LessThanTenPredicate>;

#[test]
fn add() {
    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(5).unwrap();
    let _z = x + y;
}

#[test]
fn assignment() {
    let mut _x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(6).unwrap();
    _x = y;
    assert_eq!(6, _x.as_inner());
}

#[test]
fn eq() {
    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(5).unwrap();
    assert_eq!(x, y);
}

#[test]
fn neq() {
    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(6).unwrap();
    assert_ne!(x, y);
}

#[test]
fn ord() {
    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(6).unwrap();
    assert!(x < y);
}
