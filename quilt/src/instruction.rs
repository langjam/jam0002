#[derive(Debug, PartialEq)]
pub enum Instruction {
    PushA,       // push tape[registerA] to the stack
    PopUntil,    // pops until hitting 0
    Push,        // pushes a u16 onto the stack
    Save,        // saves u16 into tape[registerA]
    MovA,        // moves an address into the registerA
    PopA,        // pops the stack and saves into tape[registerA]
    Add,         // pops the stack twice, adds the numbers & pushes the result
    Sub,         // pops the stack twice, subs the numbers & pushes the result
    Mult,        // pops the stack twice, multiplies the numbers & pushes the result
    Div,         // pops the stack twice, divides the numbers & pushes the result
    Road,        // where the program goes
    LeftShift,   // pops the stack once, shifts the number left one & pushes the result
    RightShift,  // pops the stack once, shifts the number right one & pushes the result
    And,         // pops the stack twice, bitwise ands the numbers & pushes the result
    Or,          // pops the stack twice, bitwise ors the numbers & pushes the result
    Not,         // pops the stack once, bitwise negates the number & pushes the result
    Xor,         // pops the stack twice, bitwise xors the numbers & pushes the result
    Output,      // outputs & pops the top of the stack to stdout
    OutputUntil, // outputs & pops the top of the stack to stdout until a 0 is reached
    Modulo,      // pops the stack twice, divides the numbers & pushes the remainder
    Start,       // where the program starts

    None, // just data
}

impl Instruction {
    pub fn takes_arg(&self) -> bool {
        matches!(self, Self::Push | Self::MovA | Self::Save)
    }

    pub fn is_conditional(&self) -> bool {
        matches!(self, Self::PopUntil | Self::OutputUntil)
    }
}
