use crate::{AgentState, Evaluate};

macro_rules! integer_to_literal_primitive {
    ($i:literal) => {
        $crate::ast::Expression::Literal($crate::ast::Primitive::Integer($i))
    };
}

macro_rules! generate_move_command_assertion_from_literal {
    ($behavior:expr, $steps:literal => $x:literal, $y:literal) => {
        assert_eq!(
            AgentState::default()
                .with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))])
                .with_pc(1)
                .with_coordinates(Coordinates($x, $y)),
            MoveCmd($behavior, $steps)
                .evaluate(AgentState::default().with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))]))
        );
    };
    ($($behavior:expr, $steps:literal => $x:literal, $y:literal,)*) => {
        $(
        generate_move_command_assertion_from_literal!($behavior, $steps=> $x, $y);
        )*
    };
    ($behavior:expr, $steps:literal to $direction:expr => $x:literal, $y:literal) => {
        assert_eq!(
            AgentState::default()
                .with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))])
                .with_pc(1)
                .with_coordinates(Coordinates($x, $y))
                .with_direction($direction),
            MoveCmd($behavior, $steps)
                .evaluate(AgentState::default().with_direction($direction).with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))]))
        );
    };
    ($($behavior:expr, $steps:literal to $direction:expr => $x:literal, $y:literal,)*) => {
        $(
        generate_move_command_assertion_from_literal!($behavior, $steps to $direction => $x, $y);
        )*
    };
    ($behavior:expr, $steps:literal to $direction:expr => $x:literal, $y:literal with $new_direction:expr) => {
        assert_eq!(
            AgentState::default()
                .with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))])
                .with_pc(1)
                .with_coordinates(Coordinates($x, $y))
                .with_direction($new_direction),
            MoveCmd($behavior, $steps)
                .evaluate(AgentState::default().with_direction($direction).with_commands(vec![Command::Move(integer_to_literal_primitive!($steps))]))
        );
    };
    ($($behavior:expr, $steps:literal to $direction:expr => $x:literal, $y:literal with $new_direction:expr,)*) => {
        $(
        generate_move_command_assertion_from_literal!($behavior, $steps to $direction => $x, $y with $new_direction);
        )*
    };
}

macro_rules! generate_move_command_assertion_from_expresssion {
    ($behavior:ty, $steps:expr, $direction:expr => $x:literal, $y:literal with $new_direction:expr) => {
        assert_eq!(
            AgentState::default()
                .with_commands(vec![Command::Move($steps.clone())])
                .with_pc(1)
                .with_coordinates(Coordinates($x, $y))
                .with_direction($new_direction),
            Evaluate::<$behavior, _>::evaluate(
                Command::Move($steps.clone()),
                AgentState::default().with_direction($direction).with_commands(vec![Command::Move($steps)])
            )
        );
    };
    ($($behavior:ty, $steps:expr, $direction:expr => $x:literal, $y:literal with $new_direction:expr,)*) => {
        $(
        generate_move_command_assertion_from_expresssion!($behavior, $steps, $direction => $x, $y with $new_direction);
        )*
    };


}

#[test]
fn should_generate_expected_new_coordinates_for_move_when_wrapped() {
    use crate::ast::{Command, Direction, Expression, Primitive};
    use crate::{Coordinates, MoveCmd, WrapOnOverflow};

    generate_move_command_assertion_from_literal!(
        WrapOnOverflow, 5 => 0, 5,
        WrapOnOverflow, 101 => 0, 1,
    );

    generate_move_command_assertion_from_literal!(
        WrapOnOverflow, 49 to Direction::N => 0, 51,
        WrapOnOverflow, 49 to Direction::W => 51, 0,
        WrapOnOverflow, 101 to Direction::E => 1, 0,
    );

    let addition_expr = Expression::Add(
        Box::new(Expression::Literal(Primitive::Integer(3))),
        Box::new(Expression::Literal(Primitive::Integer(4))),
    );
    let subtraction_expr = Expression::Sub(
        Box::new(Expression::Literal(Primitive::Integer(4))),
        Box::new(Expression::Literal(Primitive::Integer(3))),
    );

    generate_move_command_assertion_from_expresssion!(
        WrapOnOverflow, addition_expr, Direction::N => 0, 93 with Direction::N,
        WrapOnOverflow, subtraction_expr, Direction::N => 0, 99  with Direction::N,
    );
}

#[test]
fn should_generate_expected_new_coordinates_for_move_when_reflected() {
    use crate::ast::{Command, Direction, Expression, Primitive};
    use crate::{Coordinates, MoveCmd, ReflectOnOverflow};

    generate_move_command_assertion_from_literal!(
        ReflectOnOverflow, 6 to Direction::N => 0, 6 with Direction::S,
        ReflectOnOverflow, 100 to Direction::S => 0, 98 with Direction::N,
        ReflectOnOverflow, 6 to Direction::W => 6, 0 with Direction::E,
        ReflectOnOverflow, 100 to Direction::E => 98, 0 with Direction::W,
        ReflectOnOverflow, 6 to Direction::NW => 6, 6 with Direction::SE,
        ReflectOnOverflow, 100 to Direction::SE => 98, 98 with Direction::NW,
    );

    let addition_expr = Expression::Add(
        Box::new(Expression::Literal(Primitive::Integer(3))),
        Box::new(Expression::Literal(Primitive::Integer(4))),
    );
    let subtraction_expr = Expression::Sub(
        Box::new(Expression::Literal(Primitive::Integer(4))),
        Box::new(Expression::Literal(Primitive::Integer(3))),
    );

    generate_move_command_assertion_from_expresssion!(
        ReflectOnOverflow, addition_expr, Direction::N => 0, 7 with Direction::S,
        ReflectOnOverflow, subtraction_expr, Direction::N => 0, 1 with Direction::S,
    );
}

#[test]
fn should_panic_on_negative_evaluated_expression_in_move_cmd() {
    use crate::ast::{Command, Direction, Expression, Primitive};
    use crate::{EvaluateMut, ReflectOnOverflow};

    let subtraction_expr = Expression::Sub(
        Box::new(Expression::Literal(Primitive::Integer(3))),
        Box::new(Expression::Literal(Primitive::Integer(5))),
    );

    assert!(EvaluateMut::<ReflectOnOverflow, _>::evaluate_mut(
        &mut AgentState::default()
            .with_direction(Direction::N)
            .with_commands(vec![Command::Move(subtraction_expr.clone())]),
        Command::Move(subtraction_expr)
    )
    .is_err());
}

#[test]
fn should_allow_expressions_in_assignment() {
    use crate::ast::{Command, Expression, Primitive};
    use crate::SetVariableCmd;

    // literals
    let var_name = "test";
    let literal_expr = Expression::Literal(Primitive::Integer(5));
    let cmd = Command::SetVariable(var_name.to_string(), literal_expr.clone());
    assert_eq!(
        AgentState::default()
            .with_commands(vec![cmd.clone()])
            .with_pc(1)
            .with_variable(var_name, Primitive::Integer(5)),
        Evaluate::<crate::WrapOnOverflow, _>::evaluate(
            SetVariableCmd(var_name.to_string(), literal_expr),
            AgentState::default().with_commands(vec![cmd]),
        )
    );

    // arithmetic expressions
    let arithmetic_expr = Expression::Add(
        Box::new(Expression::Literal(Primitive::Integer(5))),
        Box::new(Expression::Literal(Primitive::Integer(4))),
    );
    let cmd = Command::SetVariable(var_name.to_string(), arithmetic_expr.clone());
    assert_eq!(
        AgentState::default()
            .with_commands(vec![cmd.clone()])
            .with_pc(1)
            .with_variable(var_name, Primitive::Integer(9)),
        Evaluate::<crate::WrapOnOverflow, _>::evaluate(
            SetVariableCmd(var_name.to_string(), arithmetic_expr,),
            AgentState::default().with_commands(vec![cmd]),
        )
    );

    // assignment expressions
    let assignment_expr = Expression::Add(
        Box::new(Expression::GetVariable(var_name.to_string())),
        Box::new(Expression::Literal(Primitive::Integer(5))),
    );

    let cmd = Command::SetVariable(var_name.to_string(), assignment_expr.clone());
    assert_eq!(
        AgentState::default()
            .with_commands(vec![cmd.clone()])
            .with_pc(1)
            .with_variable(var_name, Primitive::Integer(9)),
        Evaluate::<crate::WrapOnOverflow, _>::evaluate(
            SetVariableCmd(var_name.to_string(), assignment_expr,),
            AgentState::default()
                .with_commands(vec![cmd])
                .with_variable(var_name, Primitive::Integer(4)),
        )
    )
}

#[test]
fn should_jump_expressions_in_assignment() {
    use crate::ast::{Command, Expression, Primitive};

    let var_name = "test";

    let cmd = Command::JumpTrue(
        5,
        Expression::Equals(
            Box::new(Expression::GetVariable(var_name.to_string())),
            Box::new(Expression::Literal(Primitive::Integer(1))),
        ),
    );
    assert_eq!(
        AgentState::default()
            .with_commands(vec![cmd.clone()])
            .with_pc(5)
            .with_variable("test", Primitive::Integer(1)),
        Evaluate::<crate::WrapOnOverflow, _>::evaluate(
            cmd.clone(),
            AgentState::default()
                .with_commands(vec![cmd])
                .with_variable("test", Primitive::Integer(1)),
        )
    )
}

#[test]
fn should_evaluate_expressions_to_correct_value() {
    use crate::ast::{Expression, Primitive};
    use crate::EvaluateMut;

    let expressions = vec![
        // literals
        (Expression::Literal(Primitive::Integer(5)), 5),
        // positive evaluating arithmetic
        (
            Expression::Add(
                Box::new(Expression::Literal(Primitive::Integer(5))),
                Box::new(Expression::Literal(Primitive::Integer(4))),
            ),
            9,
        ),
        // negative evaluating arithmetic
        (
            Expression::Add(
                Box::new(Expression::Literal(Primitive::Integer(-5))),
                Box::new(Expression::Literal(Primitive::Integer(4))),
            ),
            -1,
        ),
        (
            Expression::Sub(
                Box::new(Expression::Literal(Primitive::Integer(-4))),
                Box::new(Expression::Literal(Primitive::Integer(-5))),
            ),
            1,
        ),
    ];

    for (expr, expected_value) in expressions {
        assert_eq!(
            Ok(Primitive::Integer(expected_value)),
            EvaluateMut::<crate::WrapOnOverflow, _>::evaluate_mut(&mut AgentState::default(), expr,),
        );
    }
}
