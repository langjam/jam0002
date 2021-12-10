/// A program is composed of zero or more agent scripts.
#[derive(Debug)]
pub struct Program(Vec<Agent>);

impl Program {
    /// initializes a new program from a list of agents
    pub fn new(agents: Vec<Agent>) -> Self {
        Self(agents)
    }

    /// Returns a borrowed slice of agents.
    pub fn agents(&self) -> &[Agent] {
        &self.0
    }
}

impl From<Program> for Vec<Agent> {
    fn from(program: Program) -> Self {
        program.0
    }
}

/// An agent is represented by a list of commands.
#[derive(Debug, Clone, PartialEq)]
pub struct Agent {
    pub commands: Vec<Command>,
}

impl Agent {
    pub fn new(commands: Vec<Command>) -> Self {
        Self { commands }
    }
}

/// All valid Command variants that an Agent can perform.
#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    /// Sets a variable identified by the string to the primitive evaluated to by
    /// the associated Expression.
    SetVariable(String, Expression),
    /// Defines the direction an agent should face.
    Face(Direction),
    /// Turns by a number of rotations where a positive number represents a
    /// clockwise rotation and a negavite represents a counter-clockwise
    /// rotation.
    Turn(Expression),
    /// Move specifies the steps that an agent will move in the direction it is
    /// facing.
    Move(Expression),
    /// Goto jumps to the enclosed offset in an agents command list.
    Goto(u32),
    /// Like Goto, JumpTrue jumps to the enclosed offset if the passed conditional
    /// expression evaluates to true.
    JumpTrue(u32, Expression),
}

/// Expresion covers the simple expression operations that can evaluated in
/// some agent commands.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Evaluates to a Literal Primitive. ex. `5`
    Literal(Primitive),
    /// Evaluates equality on two evaluated expressions. ex.`5 == 5`
    Equals(Box<Expression>, Box<Expression>),
    /// References a variable. ex `a`
    GetVariable(String),
    /// Evaluates the sum of two expressions. ex. `5 + 5`
    Add(Box<Expression>, Box<Expression>),
    /// Evaluates the difference of two expressions. ex. `5 - 5`
    Sub(Box<Expression>, Box<Expression>),
    /// Evaluates the product of two expressions. ex. `5 * 5`
    Mul(Box<Expression>, Box<Expression>),
    /// Evaluates the quotient of two expressions. ex. `5 / 5`
    Div(Box<Expression>, Box<Expression>),
}

/// Represents the cardinal directions that an agent can face.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum Direction {
    N = 0,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}

impl Direction {
    pub fn invert_x(self) -> Self {
        match self {
            Direction::NE => Direction::NW,
            Direction::E => Direction::W,
            Direction::SE => Direction::SW,
            Direction::SW => Direction::SE,
            Direction::W => Direction::E,
            Direction::NW => Direction::NE,
            other => other,
        }
    }

    pub fn invert_y(self) -> Self {
        match self {
            Direction::NE => Direction::SE,
            Direction::SE => Direction::NE,
            Direction::SW => Direction::NW,
            Direction::NW => Direction::SW,
            Direction::N => Direction::S,
            Direction::S => Direction::N,
            other => other,
        }
    }

    pub fn invert_xy(self) -> Self {
        match self {
            Direction::NE => Direction::SW,
            Direction::SE => Direction::NW,
            Direction::SW => Direction::NE,
            Direction::NW => Direction::SE,
            Direction::N => Direction::S,
            Direction::S => Direction::N,
            Direction::E => Direction::W,
            Direction::W => Direction::E,
        }
    }
}

impl From<i32> for Direction {
    fn from(src: i32) -> Self {
        match (src % 8).abs() {
            0 => Self::N,
            1 => Self::NE,
            2 => Self::E,
            3 => Self::SE,
            4 => Self::S,
            5 => Self::SW,
            6 => Self::W,
            7 => Self::NW,
            _ => unreachable!(),
        }
    }
}

/// Valid primititive types for the language, currently only integer or boolean.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Primitive {
    Integer(i32),
    Boolean(bool),
}
