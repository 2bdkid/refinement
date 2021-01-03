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
