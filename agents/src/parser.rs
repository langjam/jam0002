use parcel::parsers::character::*;
use parcel::prelude::v1::*;

use crate::ast;

#[derive(Debug)]
enum Statements {
    Label(String),
    Comment,
    Command(ParsedCommand),
}

#[derive(Debug, PartialEq)]
enum ParsedCommand {
    SetVariable(String, ast::Expression),
    Face(ast::Direction),
    Turn(ast::Expression),
    Move(ast::Expression),
    Goto(String),
    JumpTrue(String, ast::Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErr {
    UndefinedLabel(String),
    Unspecified(String),
}

enum AgentOrComment {
    Comment,
    Agent(ast::Agent),
}

#[allow(dead_code)]
pub fn parse(source: &str) -> Result<ast::Program, ParseErr> {
    let input: Vec<(usize, char)> = source.chars().enumerate().collect();

    let res = parcel::one_or_more(parcel::right(parcel::join(
        parcel::zero_or_more(newline_terminated_whitespace()),
        comment()
            .map(|_| AgentOrComment::Comment)
            .or(|| agent().map(AgentOrComment::Agent)),
    )))
    .map(|aoc| {
        aoc.into_iter().fold(Vec::new(), |mut acc, aoc| {
            if let AgentOrComment::Agent(agent) = aoc {
                acc.push(agent);
                acc
            } else {
                acc
            }
        })
    })
    .map(ast::Program::new)
    .parse(&input)
    .map_err(ParseErr::Unspecified)
    .and_then(|ms| match ms {
        MatchStatus::Match {
            span: _,
            remainder: _,
            inner,
        } => Ok(inner),
        MatchStatus::NoMatch(_) => Err(ParseErr::Unspecified("not a valid program".to_string())),
    });
    res
}

fn agent<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Agent> {
    use std::collections::HashMap;
    move |input: &'a [(usize, char)]| {
        let (span, remainder, command_or_labels) = parcel::right(parcel::join(
            expect_str("agent "),
            parcel::join(label(), parcel::zero_or_more(statement())),
        ))
        .parse(input)
        .map_err(ParseErr::Unspecified)
        .and_then(|ms| match ms {
            MatchStatus::Match {
                span,
                remainder,
                inner,
            } => Ok((span, remainder, inner.1)),
            MatchStatus::NoMatch(_) => {
                Err(ParseErr::Unspecified("not a valid program".to_string()))
            }
        })
        .map_err(|e| format!("{:?}", e))?;

        let labels = command_or_labels
            .iter()
            .fold(
                (HashMap::new(), 0usize),
                |(mut labels, idx), col| match col {
                    Statements::Label(id) => {
                        labels.insert(id.clone(), idx);
                        (labels, idx)
                    }
                    Statements::Command(_) => (labels, idx + 1),
                    Statements::Comment => (labels, idx),
                },
            )
            // Index isn't needeed after calculating the labels.
            .0;

        command_or_labels
            .into_iter()
            .map(|col| match col {
                Statements::Label(_) | Statements::Comment => None,
                Statements::Command(pc) => Some(pc),
            })
            .flatten()
            .map(|pc| match pc {
                ParsedCommand::SetVariable(id, expr) => Ok(ast::Command::SetVariable(id, expr)),
                ParsedCommand::Face(direction) => Ok(ast::Command::Face(direction)),
                ParsedCommand::Turn(rotations) => Ok(ast::Command::Turn(rotations)),
                ParsedCommand::Move(distance) => Ok(ast::Command::Move(distance)),
                ParsedCommand::Goto(label) => {
                    if let Some(&offset) = labels.get(&label) {
                        Ok(ast::Command::Goto(offset as u32))
                    } else {
                        Err(ParseErr::UndefinedLabel(label))
                    }
                }
                ParsedCommand::JumpTrue(label, expr) => {
                    if let Some(&offset) = labels.get(&label) {
                        Ok(ast::Command::JumpTrue(offset as u32, expr))
                    } else {
                        Err(ParseErr::UndefinedLabel(label))
                    }
                }
            })
            .collect::<Result<Vec<ast::Command>, ParseErr>>()
            .map_err(|e| format!("{:?}", e))
            .map(|commands| parcel::MatchStatus::Match {
                span,
                remainder,
                inner: ast::Agent::new(commands),
            })
    }
}

fn statement<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], Statements> {
    parcel::right(parcel::join(
        parcel::one_or_more(non_newline_whitespace()),
        command()
            .map(Statements::Command)
            .or(|| comment().map(|_| Statements::Comment))
            .or(|| label().map(Statements::Label)),
    ))
}

fn label<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], String> {
    parcel::left(parcel::join(
        identifier(),
        parcel::join(expect_character(':'), newline_terminated_whitespace()),
    ))
}

fn comment<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ()> {
    parcel::right(parcel::join(
        expect_character('#'),
        parcel::left(parcel::join(
            parcel::zero_or_more(any_non_whitespace_character().or(non_newline_whitespace))
                .map(|_| ()),
            newline_terminated_whitespace(),
        )),
    ))
}

fn command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::left(parcel::join(
        set_command()
            .or(face_command)
            .or(move_command)
            .or(turn_command)
            .or(goto_command)
            .or(jump_true_command),
        parcel::join(parcel::one_or_more(non_newline_whitespace()), comment())
            .map(|_| '\n')
            .or(newline_terminated_whitespace),
    ))
}

fn set_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(
        expect_str("set "),
        parcel::join(
            identifier(),
            parcel::right(parcel::join(
                non_newline_whitespace_wrapped(expect_character('=')),
                expression(),
            )),
        ),
    ))
    .map(|(id, expr)| ParsedCommand::SetVariable(id, expr))
}

fn jump_true_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(
        expect_str("jump to "),
        parcel::join(
            identifier(),
            parcel::right(parcel::join(
                non_newline_whitespace_wrapped(expect_str("if")),
                parcel::join(
                    expression(),
                    parcel::right(parcel::join(
                        non_newline_whitespace_wrapped(expect_str("is")),
                        expression(),
                    )),
                ),
            )),
        ),
    ))
    .map(|(id, (lhs, rhs))| {
        ParsedCommand::JumpTrue(id, ast::Expression::Equals(Box::new(lhs), Box::new(rhs)))
    })
}

fn move_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(expect_str("move "), expression())).map(ParsedCommand::Move)
}

fn face_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(expect_str("face "), direction())).map(ParsedCommand::Face)
}

fn turn_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(expect_str("turn "), expression())).map(ParsedCommand::Turn)
}

fn goto_command<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ParsedCommand> {
    parcel::right(parcel::join(expect_str("goto "), identifier())).map(ParsedCommand::Goto)
}

fn direction<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Direction> {
    use ast::Direction;

    parcel::one_of(vec![
        expect_str("NW"),
        expect_str("nw"),
        expect_str("NE"),
        expect_str("ne"),
        expect_str("SW"),
        expect_str("sw"),
        expect_str("SE"),
        expect_str("se"),
        expect_str("N"),
        expect_str("n"),
        expect_str("W"),
        expect_str("w"),
        expect_str("S"),
        expect_str("s"),
        expect_str("E"),
        expect_str("e"),
    ])
    .map(|direction| match direction.as_str() {
        "N" | "n" => Direction::N,
        "E" | "e" => Direction::E,
        "S" | "s" => Direction::S,
        "W" | "w" => Direction::W,
        "NW" | "nw" => Direction::NW,
        "NE" | "ne" => Direction::NE,
        "SW" | "sw" => Direction::SW,
        "SE" | "se" => Direction::SE,
        _ => unreachable!(),
    })
}

fn identifier<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], String> {
    parcel::one_or_more(alphabetic().or(|| digit(10)).or(|| expect_character('_')))
        .map(|chars| chars.into_iter().collect())
}

fn newline_terminated_whitespace<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], char> {
    parcel::right(parcel::join(
        parcel::zero_or_more(space().or(tab)),
        newline(),
    ))
}

const HEX_RADIX: u32 = 16;
const DEC_RADIX: u32 = 10;

fn i32_literal<'a>() -> impl Parser<'a, &'a [(usize, char)], i32> {
    move |input: &'a [(usize, char)]| {
        let preparsed_input = input;
        let res = parcel::join(
            expect_character('-').optional(),
            parcel::right(parcel::join(
                expect_str("0x"),
                parcel::one_or_more(digit(16)),
            ))
            .map(|chars| (chars, HEX_RADIX))
            .or(|| parcel::one_or_more(digit(10)).map(|chars| (chars, DEC_RADIX))),
        )
        .map(|(optional_sign, (digits, radix))| {
            let sign = optional_sign
                .map(|c| c.to_string())
                .unwrap_or_else(|| "".to_string());
            let vd: String = sign.chars().chain(digits.into_iter()).collect();
            i32::from_str_radix(&vd, radix)
        })
        .parse(input);

        match res {
            Ok(MatchStatus::Match {
                span,
                remainder,
                inner: Ok(u),
            }) => Ok(MatchStatus::Match {
                span,
                remainder,
                inner: u,
            }),

            Ok(MatchStatus::Match {
                span: _,
                remainder: _,
                inner: Err(_),
            }) => Ok(MatchStatus::NoMatch(preparsed_input)),

            Ok(MatchStatus::NoMatch(remainder)) => Ok(MatchStatus::NoMatch(remainder)),
            Err(e) => Err(e),
        }
    }
}

pub fn expression<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Expression> {
    addition()
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum AdditionExprOp {
    Plus,
    Minus,
}

#[allow(clippy::redundant_closure)]
fn addition<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Expression> {
    parcel::join(
        multiplication(),
        parcel::zero_or_more(parcel::join(
            non_newline_whitespace_wrapped(
                expect_character('+')
                    .map(|_| AdditionExprOp::Plus)
                    .or(|| expect_character('-').map(|_| AdditionExprOp::Minus)),
            ),
            non_newline_whitespace_wrapped(multiplication()),
        ))
        .map(unzip),
    )
    .map(|(first_expr, (operators, operands))| {
        operators
            .into_iter()
            .zip(operands.into_iter())
            .fold(first_expr, |lhs, (operator, rhs)| match operator {
                AdditionExprOp::Plus => ast::Expression::Add(Box::new(lhs), Box::new(rhs)),
                AdditionExprOp::Minus => ast::Expression::Sub(Box::new(lhs), Box::new(rhs)),
            })
    })
    .or(|| multiplication())
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum MultiplicationExprOp {
    Star,
    Slash,
}

#[allow(clippy::redundant_closure)]
fn multiplication<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Expression> {
    parcel::join(
        literal(),
        parcel::zero_or_more(parcel::join(
            non_newline_whitespace_wrapped(
                expect_character('*')
                    .map(|_| MultiplicationExprOp::Star)
                    .or(|| expect_character('/').map(|_| MultiplicationExprOp::Slash)),
            ),
            non_newline_whitespace_wrapped(literal()),
        ))
        .map(unzip),
    )
    .map(|(first_expr, (operators, operands))| {
        operators
            .into_iter()
            .zip(operands.into_iter())
            .fold(first_expr, |lhs, (operator, rhs)| match operator {
                MultiplicationExprOp::Star => ast::Expression::Mul(Box::new(lhs), Box::new(rhs)),
                MultiplicationExprOp::Slash => ast::Expression::Div(Box::new(lhs), Box::new(rhs)),
            })
    })
    .or(literal)
}

fn literal<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Expression> {
    boolean()
        .map(|b| ast::Expression::Literal(ast::Primitive::Boolean(b)))
        .or(|| i32_literal().map(|num| ast::Expression::Literal(ast::Primitive::Integer(num))))
        .or(|| identifier().map(ast::Expression::GetVariable))
}

fn boolean<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], bool> {
    expect_str("true")
        .map(|_| true)
        .or(|| expect_str("false").map(|_| false))
}

fn non_newline_whitespace_wrapped<'a, P, B>(parser: P) -> impl Parser<'a, &'a [(usize, char)], B>
where
    B: 'a,
    P: Parser<'a, &'a [(usize, char)], B> + 'a,
{
    parcel::right(parcel::join(
        parcel::zero_or_more(non_newline_whitespace()),
        parcel::left(parcel::join(
            parser,
            parcel::zero_or_more(non_newline_whitespace()),
        )),
    ))
}

fn unzip<A, B>(pair: Vec<(A, B)>) -> (Vec<A>, Vec<B>) {
    pair.into_iter().unzip()
}

#[cfg(test)]
mod tests {
    const TEST_PROGRAM: &str = "agent red_agent:
    set color = 0xff
    set x = 0
    set y = 0
    set direction = 0
    set a = 0
    loop:
        face NW
        move 10
        turn -4
        goto loop
        set a = 5
        jump to exit if a is 1
    exit:
agent blue_agent:
    set color = 255
    set x = 0
    set y = 0
    set direction = 0
    set a = 0
    loop:
        face NE
        move 20
        turn -30
        goto loop
        set b = 5
    exit:
";

    #[test]
    fn should_parse_agent_commands() {
        let res = crate::parser::parse(TEST_PROGRAM);

        assert_eq!(Ok(2), res.map(|program| program.agents().len()),);
    }

    #[test]
    fn should_parse_arbitrary_whitespace_before_agents() {
        use crate::{
            ast::{Agent, Command, Primitive},
            Expression,
        };

        let good_agent_input = "   

agent red_agent:
    set a = 4

agent blue_agent:
    set a = 4
";
        let res = crate::parser::parse(good_agent_input);
        let expected_cmds = vec![Command::SetVariable(
            "a".to_string(),
            Expression::Literal(Primitive::Integer(4)),
        )];

        assert_eq!(
            Ok(vec![
                Agent::new(expected_cmds.clone()),
                Agent::new(expected_cmds)
            ]),
            res.map(|program| program.agents().to_vec()),
        );

        let bad_agent_input = "   
    agent red_agent:
    set a = 4

agent blue_agent:
    set a = 4
";

        let res = crate::parser::parse(bad_agent_input);

        assert!(res.is_err());
    }

    #[test]
    fn should_parse_set_cmds_with_var_references() {
        use crate::{
            ast::{Agent, Command, Primitive},
            parser::ParseErr,
            Expression,
        };
        let set_inst = "agent red_agent:
    set a = 4
    set a = a + 5
";
        let res = crate::parser::parse(set_inst);
        let expected_cmds = vec![
            Command::SetVariable("a".to_string(), Expression::Literal(Primitive::Integer(4))),
            Command::SetVariable(
                "a".to_string(),
                Expression::Add(
                    Box::new(Expression::GetVariable("a".to_string())),
                    Box::new(Expression::Literal(Primitive::Integer(5))),
                ),
            ),
        ];

        assert_eq!(
            Ok(Agent::new(expected_cmds)),
            res.and_then(|program| program
                .agents()
                .get(0)
                .cloned()
                .ok_or_else(|| { ParseErr::Unspecified("no agents parsed".to_string()) })),
        );
    }

    #[test]
    fn should_parse_comments() {
        use crate::{
            ast::{Agent, Command, Primitive},
            parser::ParseErr,
            Expression,
        };
        let set_inst = "# top-level comment
# hello
agent red_agent:
    #
    # statement-level comment
    set a = 4 # inline comment
";
        let res = crate::parser::parse(set_inst);
        let expected_cmds = vec![Command::SetVariable(
            "a".to_string(),
            Expression::Literal(Primitive::Integer(4)),
        )];

        assert_eq!(
            Ok(Agent::new(expected_cmds)),
            res.and_then(|program| program
                .agents()
                .get(0)
                .cloned()
                .ok_or_else(|| { ParseErr::Unspecified("no agents parsed".to_string()) })),
        );
    }

    use parcel::Parser;

    #[test]
    fn should_parse_addition_expression() {
        let input: Vec<(usize, char)> = "5 + 5".chars().enumerate().collect();
        let res = crate::parser::expression().parse(&input);

        assert_eq!(
            Ok(parcel::MatchStatus::Match {
                span: 0..0,
                remainder: &input[5..],
                inner: crate::ast::Expression::Add(
                    Box::new(crate::ast::Expression::Literal(
                        crate::ast::Primitive::Integer(5)
                    )),
                    Box::new(crate::ast::Expression::Literal(
                        crate::ast::Primitive::Integer(5)
                    )),
                )
            }),
            res
        );
    }

    #[test]
    fn should_parse_multi_op_expression() {
        let input: Vec<(usize, char)> = "a * 5 + 5".chars().enumerate().collect();
        let res = crate::parser::expression().parse(&input);

        assert_eq!(
            Ok(parcel::MatchStatus::Match {
                span: 0..0,
                remainder: &input[9..],
                inner: crate::ast::Expression::Add(
                    Box::new(crate::ast::Expression::Mul(
                        Box::new(crate::ast::Expression::GetVariable("a".to_string())),
                        Box::new(crate::ast::Expression::Literal(
                            crate::ast::Primitive::Integer(5)
                        )),
                    )),
                    Box::new(crate::ast::Expression::Literal(
                        crate::ast::Primitive::Integer(5)
                    )),
                )
            }),
            res
        );
    }

    #[test]
    fn should_parse_literal_expression() {
        let input: Vec<(usize, char)> = "5".chars().enumerate().collect();
        let res = crate::parser::expression().parse(&input);

        assert_eq!(
            Ok(parcel::MatchStatus::Match {
                span: 0..0,
                remainder: &input[1..],
                inner: crate::ast::Expression::Literal(crate::ast::Primitive::Integer(5)),
            }),
            res
        );

        // hex literal
        let input: Vec<(usize, char)> = "0xF".chars().enumerate().collect();
        let res = crate::parser::expression().parse(&input);

        assert_eq!(
            Ok(parcel::MatchStatus::Match {
                span: 0..0,
                remainder: &input[3..],
                inner: crate::ast::Expression::Literal(crate::ast::Primitive::Integer(15)),
            }),
            res
        );

        let input: Vec<(usize, char)> = "a".chars().enumerate().collect();
        let res = crate::parser::expression().parse(&input);

        assert_eq!(
            Ok(parcel::MatchStatus::Match {
                span: 0..0,
                remainder: &input[1..],
                inner: crate::ast::Expression::GetVariable("a".to_string()),
            }),
            res
        );
    }
}
