struct Position {
    x: i64,
    y: i64,
}

impl Position {
    fn new(x: i64, y: i64) -> Self {
        Position { x, y }
    }
}

impl std::ops::AddAssign<&Position> for Position {
    fn add_assign(&mut self, rhs: &Position) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}
