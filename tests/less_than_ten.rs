use refinement::{Predicate, Refinement};

#[derive(PartialEq, Debug)]
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
fn add_cascading() {
    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(2).unwrap();
    let z = x.apply(|v| v + y.into_inner());
    assert_eq!(7, z.unwrap().into_inner());

    let x = LessThanTen::new(5).unwrap();
    let y = LessThanTen::new(5).unwrap();
    let z = x.apply(|v| v + y.into_inner());
    assert!(z.is_none());
}
