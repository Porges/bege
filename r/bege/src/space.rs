use std::{collections::HashMap, io::BufRead};

pub trait Position:
    Clone
    + std::cmp::Eq
    + for<'a> std::ops::Add<&'a Self, Output = Self>
    + for<'a> std::ops::AddAssign<&'a Self>
    + TryFrom<(usize, usize)>
{
}

pub fn initial_position<P: Position>() -> P {
    unsafe { P::try_from((0, 0)).unwrap_unchecked() }
}

#[inline(never)]
pub fn initial_delta<P: Position>() -> P {
    unsafe { P::try_from((1, 0)).unwrap_unchecked() }
}

// Cell is the type of numbers in the fungespace and in the stack.
pub trait Cell: num_traits::NumAssign + Copy + TryFrom<char> + Into<char> + Default {}

impl Cell for u8 {}

pub trait FungeSpace {
    type Cell: Cell;
    type Position: Position;
    fn read(&self, p: &Self::Position) -> Self::Cell;
    fn write(&mut self, p: &Self::Position, value: Self::Cell);
}

pub struct GenericFungeSpace<P, C> {
    data: HashMap<P, C>,
}

impl<P: Position, C> GenericFungeSpace<P, C> {
    pub fn new() -> Self {
        GenericFungeSpace {
            data: HashMap::new(),
        }
    }
}

impl<P: Position, C> Default for GenericFungeSpace<P, C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<P: Position + std::hash::Hash, C: Cell> FungeSpace for GenericFungeSpace<P, C> {
    type Position = P;
    type Cell = C;

    fn read(&self, p: &Self::Position) -> Self::Cell {
        #[allow(clippy::or_fun_call)] // should be a constant in all integer types
        self.data.get(p).copied().unwrap_or(
            C::try_from(' ').unwrap_or_else(|_| panic!("Type should be convertible from space")),
        )
    }

    fn write(&mut self, p: &Self::Position, value: Self::Cell) {
        self.data.insert(p.clone(), value);
    }
}

pub fn load_text<P: Position, C: TryFrom<char>>(
    reader: &mut dyn BufRead,
    space: &mut impl FungeSpace<Position = P, Cell = C>,
) -> std::io::Result<()> {
    let mut line = String::new();
    let mut y = 0usize;

    while reader.read_line(&mut line)? > 0 {
        for (x, c) in line.chars().enumerate() {
            let p = P::try_from((x, y)).unwrap_or_else(|_| todo!());
            space.write(&p, C::try_from(c).unwrap_or_else(|_| todo!()));
        }

        line.clear();
        y += 1;
    }

    Ok(())
}
