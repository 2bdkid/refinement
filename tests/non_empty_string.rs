use refinement::{Predicate, Refinement};

#[derive(PartialEq, Clone, Debug)]
struct NonEmptyStringPredicate;

impl Predicate<String> for NonEmptyStringPredicate {
    fn test(x: &String) -> bool {
        !x.is_empty()
    }
}

type NonEmptyString = Refinement<String, NonEmptyStringPredicate>;

#[test]
fn create_good_value() {
    let x = NonEmptyString::new(String::from("Hello"));
    assert_eq!(Some(String::from("Hello")), x.map(|x| x.to_inner()))
}

#[test]
fn create_bad_value() {
    let x = NonEmptyString::new(String::from(""));
    assert_eq!(None, x);
}

#[test]
fn clone_equality() {
    let x = NonEmptyString::new(String::from("Hello")).unwrap();
    assert_eq!(x, x.clone())
}
