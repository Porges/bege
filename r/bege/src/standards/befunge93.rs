use std::ops::AddAssign;

#[derive(Clone, Copy, PartialEq, Eq, std::hash::Hash)]
pub struct Position {
    x: i8,
    y: i8,
}

impl crate::space::Position for Position {}

impl TryFrom<(usize, usize)> for Position {
    type Error = <i8 as TryFrom<usize>>::Error;
    fn try_from((x, y): (usize, usize)) -> Result<Self, Self::Error> {
        Position::new(i8::try_from(x)?, i8::try_from(y)?)
    }
}

impl Position {
    pub fn new(x: i8, y: i8) -> Result<Self, std::num::TryFromIntError> {
        if x < 0 || y < 0 {
            todo!()
        }

        if x >= 80 || y >= 25 {
            todo!()
        }

        Ok(Position { x, y })
    }
}

impl std::ops::Add<&Position> for Position {
    type Output = Position;
    fn add(mut self, rhs: &Position) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl std::ops::AddAssign<&Position> for Position {
    fn add_assign(&mut self, other: &Position) {
        self.x = (self.x + other.x) % 80;
        if self.x < 0 {
            self.x += 80;
        }

        self.y = (self.y + other.y) % 25;
        if self.y < 0 {
            self.y += 25;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn position_wraps_correctly_overflow_y() {
        let mut position = Position { y: 24, x: 0 };
        position += &Position { y: 1, x: 0 };
        assert_eq!(position.y, 0);
    }

    #[test]
    fn position_wraps_correctly_underflow_y() {
        let mut position = Position { y: 0, x: 0 };
        position += &Position { y: -1, x: 0 };
        assert_eq!(position.y, 24);
    }

    #[test]
    fn position_wraps_correctly_overflow_x() {
        let mut position = Position { y: 0, x: 79 };
        position += &Position { y: 0, x: 1 };
        assert_eq!(position.x, 0);
    }

    #[test]
    fn position_wraps_correctly_underflow_x() {
        let mut position = Position { y: 0, x: 0 };
        position += &Position { y: 0, x: -1 };
        assert_eq!(position.x, 79);
    }
}
