use crate::vm::Direction::{self, East, North, South, West};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MatrixPoint(pub usize, pub usize);

impl MatrixPoint {
    pub fn neighbor(&self, direction: Direction) -> Option<Self> {
        match (direction, *self) {
            (North, Self(_, 0)) => None,
            (West, Self(0, _)) => None,
            (North, Self(x, y)) => Some(Self(x, y - 1)),
            (West, Self(x, y)) => Some(Self(x - 1, y)),
            (South, Self(x, y)) => Some(Self(x, y + 1)),
            (East, Self(x, y)) => Some(Self(x + 1, y)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Matrix<T> {
    pub matrix: Vec<Vec<T>>,
}

impl<T: Copy> Matrix<T> {
    pub fn new(matrix: Vec<Vec<T>>) -> Matrix<T> {
        Matrix { matrix }
    }

    pub fn get(&self, point: MatrixPoint) -> Option<T> {
        self.matrix.get(point.1)?.get(point.0).copied()
    }

    /// Tries to move a point in the provided direction
    /// If there is no cell in that direction, None is returned
    /// Otherwise Some(NewMatrixPoint) is returned
    pub fn go(&self, point: MatrixPoint, direction: Direction) -> Option<T> {
        point.neighbor(direction).and_then(|p| self.get(p))
    }

    pub fn corner(&self, point: MatrixPoint, dir1: Direction, dir2: Direction) -> Option<T> {
        point
            .neighbor(dir1)
            .and_then(|p| p.neighbor(dir2))
            .and_then(|p| self.get(p))
    }
}

#[cfg(test)]
mod test {
    use crate::vm::Direction;

    fn create_test_matrix() -> super::Matrix<usize> {
        let r1 = vec![1, 2, 3];
        let r2 = vec![4, 5, 6];
        let r3 = vec![7, 8, 9];
        let v = vec![r1, r2, r3];
        super::Matrix::new(v)
    }

    #[test]
    fn test_go_boundaries() {
        let m = create_test_matrix();
        assert_eq!(m.go(super::MatrixPoint(0, 0), Direction::North), None);
        assert_eq!(m.go(super::MatrixPoint(0, 0), Direction::West), None);
        assert_eq!(m.go(super::MatrixPoint(2, 2), Direction::East), None);
        assert_eq!(m.go(super::MatrixPoint(2, 2), Direction::South), None);
    }

    #[test]
    fn test_go_happy() {
        let m = create_test_matrix();
        let p = super::MatrixPoint(1, 1);

        assert_eq!(m.go(p, Direction::North).unwrap(), 2);
        assert_eq!(m.go(p, Direction::West).unwrap(), 4);
        assert_eq!(m.go(p, Direction::East).unwrap(), 6);
        assert_eq!(m.go(p, Direction::South).unwrap(), 8);
    }
}
