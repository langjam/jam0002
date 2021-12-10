use std::io;
use std::{collections::HashMap, io::Write};

use daemon::parser::{parse, Daemon};

type Messages = HashMap<String, String>;

struct Game {
    messages: Messages,
    daemon: Daemon,
    inventory: Vec<String>,
    found_friends: bool,
}

impl Game {
    fn new(messages: Messages, daemon: Daemon) -> Self {
        Self {
            messages,
            daemon,
            inventory: vec![],
            found_friends: false,
        }
    }

    fn current(&self) -> String {
        self.daemon.current()
    }

    fn messages_for_key(&mut self, line: String) -> (Vec<String>, bool) {
        let (keys, changed_state) = self.daemon.consume(line.trim().into());
        (
            keys.into_iter()
                .map(|k| {
                    self.messages
                        .get(&k)
                        .unwrap_or(&"nothing happens".into())
                        .clone()
                })
                .collect(),
            changed_state,
        )
    }

    fn execute_line(&mut self, line: String) -> Vec<String> {
        if line.trim() == "actions" {
            return self.daemon.actions();
        }

        if let None = self.daemon.htp.get(line.trim().into()) {
            return vec!["Unknown action".to_string()];
        }

        match line.trim() {
            "check_inventory" => {
                let mut inventory = self.inventory.clone();
                let (msgs, _) = self.messages_for_key(line.trim().into());
                inventory.extend(msgs);
                inventory
            }
            "visit_village" => {
                self.found_friends = true;
                self.messages_for_key(line.trim().into()).0
            }
            "visit_market" => {
                let (msgs, changed) = self.messages_for_key(line.trim().into());
                if changed {
                    self.inventory.push("Poison".to_string());
                }
                msgs
            }
            "fight_monster" => {
                let (mut msgs, _) = self.messages_for_key(line.trim().into());
                if self.found_friends
                    && self.inventory.contains(&"Sword".to_string())
                    && self.inventory.contains(&"Poison".to_string())
                {
                    msgs.push(
                        "Congratulations! You defeated the monster and found the hidden treasure!"
                            .to_string(),
                    )
                }
                msgs
            }
            "search_crates" => {
                let (msgs, changed) = self.messages_for_key(line.trim().into());
                if changed {
                    self.inventory.push("Sword".to_string());
                }
                msgs
            }
            _ => self.messages_for_key(line.trim().into()).0,
        }
    }
}

fn main() {
    let raw_messages = include_str!("messages.txt");
    let messages: Messages = raw_messages
        .lines()
        .map(|l| {
            l.split_once(char::is_whitespace)
                .map(|(s1, s2)| (s1.to_string(), s2.to_string()))
                .unwrap()
        })
        .collect();

    // depart, visit_dungeon, visit_market, explore_caves, go_boss_fight

    let source = include_str!("input.daemon");
    let daemon = parse(source);
    let mut game = Game::new(messages, daemon);

    println!("Dungeon v0.0.1\n");

    println!(
        "You stand in front of a dungeon. \
    Legends say that a huge monster guards the treasure hidden in the dungeon.
You can't just defeat it with bare hands, though you may try.
Choose your actions wisely. You can list possible actions with the 'actions' command.
    "
    );

    loop {
        print!("> ({}) ", game.current());
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read input.");

        let msgs = game.execute_line(input);
        for msg in msgs {
            println!("{}", msg)
        }
    }
}
