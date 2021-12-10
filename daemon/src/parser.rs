use std::collections::HashMap;

use nom::bytes::complete::take_while;
use nom::combinator::{all_consuming, map, recognize};
use nom::{
    self,
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{alpha1, alphanumeric1, line_ending, space0},
        streaming::space1,
    },
    multi::{many0, many1},
    sequence::{delimited, pair, preceded},
};

use crate::order::*;

pub type Result<'a, T> = nom::IResult<&'a str, T>;

type HandlerToPattern = HashMap<String, Vec<Pattern>>;

#[derive(Debug)]
pub struct Daemon {
    pub htp: HandlerToPattern,
    history: Vec<String>,
    order: Vec<String>,
    expectations: Expectations,
}

impl Daemon {
    pub fn new(htp: HandlerToPattern, order: Vec<String>, expectations: Expectations) -> Self {
        Self {
            htp,
            expectations,
            order,
            history: vec![],
        }
    }

    pub fn actions(&self) -> Vec<String> {
        self.htp.keys().cloned().collect()
    }

    pub fn current(&self) -> String {
        self.history
            .last()
            .unwrap_or(&String::from("start"))
            .clone()
    }

    pub fn anticipated_by(&self, event: &String) -> Vec<String> {
        match self.order.iter().position(|el| el == event) {
            Some(idx) => self.order[..idx].into(),
            None => vec![],
        }
    }

    pub fn find_anticipation_msgs(&self, event: &String, msgs: &mut Vec<String>) {
        let anticipated_by = self.anticipated_by(event);
        for ev in anticipated_by {
            if !self.history.contains(&ev) {
                if let Some(msg) = self.expectations.get(&(ev.clone(), event.clone())) {
                    msgs.push(msg.clone());
                }

                if let Some(msg) = self.expectations.get(&(ev.clone(), "all".into())) {
                    msgs.push(msg.clone());
                }
            }
        }
    }

    pub fn consume(&mut self, event: String) -> (Vec<String>, bool) {
        let mut msgs = vec![];
        self.find_anticipation_msgs(&event, &mut msgs);
        if msgs.len() > 0 {
            return (msgs, false);
        }

        if let Some(pats) = self.htp.get(&event) {
            for pat in pats {
                match &pat.relation {
                    TemporalRelation::After(ev) => {
                        if self.history.contains(ev) {
                            msgs.push(pat.message.clone());
                        }
                    }
                    TemporalRelation::Before(ev) => {
                        if !self.history.contains(ev) {
                            msgs.push(pat.message.clone());
                        }
                    }
                    _ => continue,
                }
            }
        }

        self.history.push(event.clone());

        (msgs, true)
    }
}

type Expectee = String;
type Expector = String;
type Expectations = HashMap<(Expector, Expectee), String>;

pub fn parse(source: &str) -> Daemon {
    let (_, handlers) = all_consuming(many1(event_handler))(source).expect("Parse failed");
    let mut order = Order::new();
    let mut preceed_all = vec![];

    let mut htp = HandlerToPattern::new();
    let mut expectations = Expectations::new();

    for handler in &handlers {
        for pattern in &handler.patterns {
            order.insert(&handler.on);
            match &pattern.relation {
                TemporalRelation::Anticipate(ev) if ev == "all" => {
                    preceed_all.push(&handler.on);
                    expectations
                        .insert((handler.on.clone(), "all".into()), pattern.message.clone());
                }
                TemporalRelation::Anticipate(ev) => {
                    expectations.insert((handler.on.clone(), ev.clone()), pattern.message.clone());
                    order.precede(&handler.on, &ev)
                }
                _ => continue,
            }
        }
        htp.insert(handler.on.clone(), handler.patterns.clone());
    }

    for ev in preceed_all {
        order.precede_all(ev);
    }

    Daemon::new(htp, order.sort(), expectations)
}

type EventName = String;

#[derive(Debug, Clone)]
pub enum TemporalRelation {
    Before(EventName),
    After(EventName),
    Anticipate(EventName),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    relation: TemporalRelation,
    message: String,
}

#[derive(Debug)]
pub struct EventHandler {
    on: EventName,
    patterns: Vec<Pattern>,
}

fn event_name(input: &str) -> Result<EventName> {
    preceded(
        space1,
        map(
            recognize(pair(alpha1, many0(alt((alphanumeric1, tag("_")))))),
            |v: &str| v.into(),
        ),
    )(input)
}

fn relation(input: &str) -> Result<TemporalRelation> {
    let (rest, rel) = alt((tag("before"), tag("after"), tag("anticipate")))(input)?;
    let (rest, name) = event_name(rest)?;
    match rel {
        "before" => Ok((rest, TemporalRelation::Before(name))),
        "after" => Ok((rest, TemporalRelation::After(name))),
        "anticipate" => Ok((rest, TemporalRelation::Anticipate(name))),
        _ => panic!("Bad temporal relation."),
    }
}

fn message(input: &str) -> Result<String> {
    let (rest, _) = space0(input)?;
    let (rest, msg) = recognize(many1(alt((alphanumeric1, tag("_")))))(rest)?;
    let (rest, _) = line_ending(rest)?;
    Ok((rest, msg.into()))
}

fn pattern(input: &str) -> Result<Pattern> {
    let (rest, relation) = relation(input)?;
    let (rest, _) = preceded(space0, tag("=>"))(rest)?;
    let (rest, message) = message(rest)?;
    Ok((rest, Pattern { relation, message }))
}

fn linespace(input: &str) -> Result<&str> {
    take_while(char::is_whitespace)(input)
}

fn patterns(input: &str) -> Result<Vec<Pattern>> {
    let (rest, _) = preceded(linespace, tag("{"))(input)?;
    let (rest, pats) = many0(delimited(linespace, pattern, linespace))(rest)?;
    let (rest, _) = tag("}")(rest)?;
    Ok((rest, pats))
}

pub fn event_handler(input: &str) -> Result<EventHandler> {
    let (rest, _) = tag("on")(input)?;
    let (rest, name) = event_name(rest)?;
    let (rest, patterns) = patterns(rest)?;
    let (rest, _) = linespace(rest)?;
    Ok((rest, EventHandler { on: name, patterns }))
}
