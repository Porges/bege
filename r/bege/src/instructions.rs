pub enum BinOp {
    Add,
    Multiply,
    Divide,
    Subtract,
    Greater,
    ReadText,
    Modulo,
}

pub enum UnOp {
    Not,
}

pub enum Instruction<P> {
    Load(P),
    Discard,
    Clear,
    Dup,
    Flip,
    OutputNumber,
    OutputChar,
    InputChar,
    BinOp(BinOp),
    UnOp(UnOp),
}
