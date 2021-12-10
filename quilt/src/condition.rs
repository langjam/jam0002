#[derive(Debug)]
pub enum Condition {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl Condition {
    pub fn compare(&self, x: i64) -> bool {
        match self {
            Self::Equal => x == 0,
            Self::NotEqual => x != 0,
            Self::Less => x < 0,
            Self::LessEqual => x <= 0,
            Self::Greater => x > 0,
            Self::GreaterEqual => x >= 0,
        }
    }
}
