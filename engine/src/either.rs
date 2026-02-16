#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}
