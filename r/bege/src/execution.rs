use crate::space::Position;

#[derive(Clone, Default)]
struct Stack<U: Clone> {
    stack: Vec<U>,
}

impl<U: Clone + Default> Stack<U> {
    pub fn new() -> Self {
        Stack { stack: Vec::new() }
    }

    pub fn push(&mut self, u: U) {
        self.stack.push(u);
    }

    pub fn pop(&mut self) -> U {
        self.stack.pop().unwrap_or_default()
    }

    pub fn clear(&mut self) {
        self.stack.clear();
    }

    pub fn dup(&mut self) {
        if self.stack.is_empty() {
            return; // no need to dup on empty
        }

        // SAFETY: just checked that the stack is not empty
        let x = unsafe { self.stack.last().unwrap_unchecked().clone() };
        self.stack.push(x);
    }

    pub fn mut_peek(&mut self) -> &mut U {
        if self.stack.is_empty() {
            self.stack.push(Default::default());
        }

        self.stack.last_mut().unwrap()
    }
}

#[derive(Clone)]
pub struct IP<P: Position, C: Clone> {
    position: P,
    delta: P,
    stack: Stack<C>,
    stacks: Stack<Stack<C>>,
}

pub fn initial_ip<P: Position, C: crate::space::Cell>() -> IP<P, C> {
    IP {
        position: crate::space::initial_position(),
        delta: crate::space::initial_delta(),
        stack: Default::default(),
        stacks: Default::default(),
    }
}

impl<P: Position, C: Clone + Default> IP<P, C> {
    pub fn new(position: P, delta: P) -> Self {
        IP {
            position,
            delta,
            stack: Default::default(),
            stacks: Default::default(),
        }
    }

    fn advance(&mut self) {
        self.position += &self.delta;
    }
}

pub fn run<P: Position, C: crate::space::Cell>(
    cursors: &[IP<P, C>],
    input: &mut dyn std::io::BufRead,
    output: &mut dyn std::io::Write,
    space: &mut impl crate::space::FungeSpace<Position = P, Cell = C>,
) {
    let mut cursors = cursors.to_vec();

    let mut remove_cursors = Vec::new();
    while !cursors.is_empty() {
        for (ix, cursor) in cursors.iter_mut().enumerate() {
            let instruction = space.read(&cursor.position);
            match apply_instruction(cursor, instruction, input, output, space) {
                None => cursor.advance(),
                Some(Halt {}) => remove_cursors.push(ix),
            }
        }

        for ix in remove_cursors.drain(..) {
            cursors.remove(ix);
        }
    }
}

fn apply_instruction<P: crate::space::Position, C: crate::space::Cell>(
    cursor: &mut IP<P, C>,
    instruction: C,
    input: &mut dyn std::io::BufRead,
    output: &mut dyn std::io::Write,
    space: &mut impl crate::space::FungeSpace<Position = P, Cell = C>,
) -> Option<Halt> {
    match instruction.into() {
        '$' => {
            cursor.stack.pop();
        }
        ':' => {
            cursor.stack.dup();
        }
        'n' => {
            // TODO: check befunge level
            cursor.stack.clear();
        }
        '\\' => {
            let l = cursor.stack.pop();
            let r = cursor.stack.pop();
            cursor.stack.push(l);
            cursor.stack.push(r);
        }
        '0'..='9' => {
            cursor
                .stack
                .push(instruction - unsafe { C::try_from('0').unwrap_unchecked() });
        }
        '+' => {
            let r = cursor.stack.pop();
            *cursor.stack.mut_peek() += r;
        }
        '-' => {
            let r = cursor.stack.pop();
            let l = cursor.stack.pop();
            cursor.stack.push(l - r);
        }
        '*' => {
            let r = cursor.stack.pop();
            let l = cursor.stack.pop();
            cursor.stack.push(l * r);
        }
        '/' => {
            let r = cursor.stack.pop();
            let l = cursor.stack.pop();
            cursor.stack.push(l / r);
        }
        '@' => {
            return Some(Halt {});
        }
        '!' => {
            if cursor.stack.pop().is_zero() {
                cursor.stack.push(C::one());
            } else {
                cursor.stack.push(C::zero());
            }
        }
        ',' => {
            let v: char = cursor.stack.pop().into();
            write!(output, "{}", v).unwrap();
        }
        '\'' => {
            let v = space.read(&(cursor.position.clone() + &cursor.delta));
            cursor.stack.push(v);
            cursor.advance();
        }
        _ => {}
    }

    None
}

struct Halt {}
