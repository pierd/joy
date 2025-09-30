#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn left(l: L) -> Self {
        Self::Left(l)
    }

    pub fn right(r: R) -> Self {
        Self::Right(r)
    }
}
