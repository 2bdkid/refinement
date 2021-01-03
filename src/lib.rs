//! Convenient creation of type-safe refinement types.

use std::borrow::Borrow;
use std::convert::AsRef;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{
    Add, BitAnd, BitOr, BitXor, Bound, Div, Index, Mul, Neg, Not, RangeBounds, Rem, Shl, Shr, Sub,
};

/// A `Predicate` tests if a value satisfies a particular refinement type.
///
/// # Example
/// ```
/// use refinement::Predicate;
///
/// struct LessThanTenPredicate;
///
/// impl Predicate<i32> for LessThanTenPredicate {
///     fn test(x: &i32) -> bool {
///         *x < 10
///     }
/// }
///
/// ```
pub trait Predicate<T> {
    fn test(x: &T) -> bool;
}

/// A `Refinement` type ensures all values of a particular type satisfy a `Predicate`.
///
/// Use [`to_inner`](Refinement::to_inner) to access the underlying value or [`into_inner`](Refinement::into_inner) to unwrap the value.
/// `Refinement` also implements many common standard library traits if the underlying value also
/// implements them.
///
/// # Examples
/// ```
/// use refinement::{Predicate, Refinement};
///
/// struct LessThanTenPredicate;
///
/// impl Predicate<i32> for LessThanTenPredicate {
///     fn test(x: &i32) -> bool {
///         *x < 10
///     }
/// }
///
/// type LessThanTen = Refinement<i32, LessThanTenPredicate>;
///
/// let x = LessThanTen::new(5);
/// assert_eq!(Some(5), x.map(|x| x.to_inner()));
///
/// let y = LessThanTen::new(11);
/// assert!(y.is_none());
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Refinement<T, P>(T, PhantomData<P>);

impl<T, P> Refinement<T, P>
where
    P: Predicate<T>,
{
    /// Create a refined value from the underlying type `T`.
    ///
    /// Returns `x` under the refinement type if `x` satisfies `P`, otherwise returns [`None`](std::option::Option::None).
    ///
    /// # Examples
    ///
    /// ```
    /// use refinement::{Predicate, Refinement};
    ///
    /// struct NonEmptyStringPredicate;
    ///
    /// impl Predicate<String> for NonEmptyStringPredicate {
    ///     fn test(x: &String) -> bool {
    ///        !x.is_empty()
    ///     }
    /// }
    ///
    /// type NonEmptyString = Refinement<String, NonEmptyStringPredicate>;
    ///
    /// let s1 = NonEmptyString::new(String::from("Hello"));
    /// assert!(s1.is_some());
    ///
    /// let s2 = NonEmptyString::new(String::from(""));
    /// assert!(s2.is_none());
    /// ```
    pub fn new(x: T) -> Option<Self> {
        if P::test(&x) {
            Some(Refinement(x, PhantomData))
        } else {
            None
        }
    }

    /// Unwrap the underlying value
    ///
    /// # Examples
    ///
    /// ```
    /// use refinement::{Predicate, Refinement};
    ///
    /// struct ThreeDigitStringPredicate;
    ///
    /// impl Predicate<String> for ThreeDigitStringPredicate {
    ///     fn test(x: &String) -> bool {
    ///        x.chars().count() == 3 && x.chars().filter(|c| c.is_ascii_digit()).count() == 3
    ///     }
    /// }
    ///
    /// type ThreeDigitString = Refinement<String, ThreeDigitStringPredicate>;
    ///
    /// let s = ThreeDigitString::new(String::from("123")).unwrap();
    ///
    /// assert_eq!(String::from("123"), s.into_inner());
    /// ```
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T, P> Refinement<T, P>
where
    T: Clone,
    P: Predicate<T>,
{
    /// Retrieve the underlying value without consuming `self`.
    ///
    /// See [`into_inner`](Refinement::into_inner) for a similar usage pattern.
    pub fn to_inner(&self) -> T {
        self.0.clone()
    }
}

impl<T, P> Borrow<T> for Refinement<T, P>
where
    P: Predicate<T>,
{
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T, P> AsRef<T> for Refinement<T, P>
where
    P: Predicate<T>,
{
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T, P> Add<T> for Refinement<T, P>
where
    T: Add<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn add(self, rhs: T) -> Self::Output {
        Self::new(self.0 + rhs)
    }
}

impl<T, P> Add for Refinement<T, P>
where
    T: Add<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0)
    }
}

impl<T, P> BitAnd<T> for Refinement<T, P>
where
    T: BitAnd<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitand(self, rhs: T) -> Self::Output {
        Self::new(self.0 & rhs)
    }
}

impl<T, P> BitAnd for Refinement<T, P>
where
    T: BitAnd<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self::new(self.0 & rhs.0)
    }
}

impl<T, P> BitOr<T> for Refinement<T, P>
where
    T: BitOr<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitor(self, rhs: T) -> Self::Output {
        Self::new(self.0 | rhs)
    }
}

impl<T, P> BitOr for Refinement<T, P>
where
    T: BitOr<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self::new(self.0 | rhs.0)
    }
}

impl<T, P> BitXor<T> for Refinement<T, P>
where
    T: BitXor<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitxor(self, rhs: T) -> Self::Output {
        Self::new(self.0 ^ rhs)
    }
}

impl<T, P> BitXor for Refinement<T, P>
where
    T: BitXor<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::new(self.0 ^ rhs.0)
    }
}

impl<T, P> Div<T> for Refinement<T, P>
where
    T: Div<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn div(self, rhs: T) -> Self::Output {
        Self::new(self.0 / rhs)
    }
}

impl<T, P> Div for Refinement<T, P>
where
    T: Div<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.0 / rhs.0)
    }
}

impl<T, P, I> Index<I> for Refinement<T, P>
where
    T: Index<I>,
    P: Predicate<T>,
{
    type Output = T::Output;
    fn index(&self, index: I) -> &Self::Output {
        self.0.index(index)
    }
}

impl<T, P> Mul<T> for Refinement<T, P>
where
    T: Mul<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn mul(self, rhs: T) -> Self::Output {
        Self::new(self.0 * rhs)
    }
}

impl<T, P> Mul for Refinement<T, P>
where
    T: Mul<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.0 * rhs.0)
    }
}

impl<T, P> Neg for Refinement<T, P>
where
    T: Neg<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn neg(self) -> Self::Output {
        Self::new(self.0.neg())
    }
}

impl<T, P> Not for Refinement<T, P>
where
    T: Not<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn not(self) -> Self::Output {
        Self::new(self.0.not())
    }
}

impl<T, P, B> RangeBounds<B> for Refinement<T, P>
where
    T: RangeBounds<B>,
    P: Predicate<T>,
{
    fn start_bound(&self) -> Bound<&B> {
        self.0.start_bound()
    }

    fn end_bound(&self) -> Bound<&B> {
        self.0.end_bound()
    }
}

impl<T, P> Rem<T> for Refinement<T, P>
where
    T: Rem<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn rem(self, rhs: T) -> Self::Output {
        Self::new(self.0 % rhs)
    }
}

impl<T, P> Rem for Refinement<T, P>
where
    T: Rem<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn rem(self, rhs: Self) -> Self::Output {
        Self::new(self.0 % rhs.0)
    }
}

impl<T, P> Shl<T> for Refinement<T, P>
where
    T: Shl<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn shl(self, rhs: T) -> Self::Output {
        Self::new(self.0 << rhs)
    }
}

impl<T, P> Shl for Refinement<T, P>
where
    T: Shl<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn shl(self, rhs: Self) -> Self::Output {
        Self::new(self.0 << rhs.0)
    }
}

impl<T, P> Shr<T> for Refinement<T, P>
where
    T: Shr<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn shr(self, rhs: T) -> Self::Output {
        Self::new(self.0 >> rhs)
    }
}

impl<T, P> Shr for Refinement<T, P>
where
    T: Shr<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn shr(self, rhs: Self) -> Self::Output {
        Self::new(self.0 >> rhs.0)
    }
}

impl<T, P> Sub<T> for Refinement<T, P>
where
    T: Sub<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn sub(self, rhs: T) -> Self::Output {
        Self::new(self.0 - rhs)
    }
}

impl<T, P> Sub for Refinement<T, P>
where
    T: Sub<Output = T>,
    P: Predicate<T>,
{
    type Output = Option<Self>;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0)
    }
}

impl<T, P> fmt::Display for Refinement<T, P>
where
    T: fmt::Display,
    P: Predicate<T>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
