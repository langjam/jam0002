use multimap::MultiMap;
use std::collections::HashSet;

type Event = String;
type Events = MultiMap<Event, Event>;

#[derive(Debug)]
pub struct Order {
    events: Events,
    nodes: HashSet<Event>,
}

impl Order {
    pub fn new() -> Self {
        Self {
            events: MultiMap::new(),
            nodes: HashSet::new(),
        }
    }

    pub fn insert(&mut self, event: &Event) {
        self.nodes.insert(event.clone());
    }

    pub fn precede(&mut self, event_a: &Event, event_b: &Event) {
        self.events.insert(event_a.clone(), event_b.clone())
    }

    pub fn precede_all(&mut self, event: &Event) {
        for node in &self.nodes.clone() {
            if node != event {
                self.precede(event, node);
            }
        }
    }

    fn has_parent(&self, node: &Event) -> bool {
        self.events
            .iter_all()
            .any(|(_, children)| children.contains(node))
    }

    fn has_parents_except(&self, node: &Event, except: &Event) -> bool {
        self.events
            .iter_all()
            .any(|(parent, children)| parent != except && children.contains(node))
    }

    fn no_incoming_edges(&self) -> Vec<Event> {
        let mut no_incoming_edge = vec![];
        for node in self.events.keys() {
            if !self.has_parent(node) {
                no_incoming_edge.push(node.clone());
            }
        }
        no_incoming_edge
    }

    pub fn sort(&mut self) -> Vec<Event> {
        let mut sorted = vec![];
        let mut no_incoming_edge = self.no_incoming_edges();

        while !no_incoming_edge.is_empty() {
            let node = no_incoming_edge.pop().unwrap();
            let default = Vec::new();
            let children = self.events.get_vec(&node).unwrap_or(&default);
            for child in children {
                if !self.has_parents_except(child, &node) {
                    no_incoming_edge.push(child.clone());
                }
            }
            self.events.remove(&node);
            sorted.push(node);
        }

        if self.events.keys().len() > 0 {
            panic!("Bad order. Your dependency graph has a cycle.")
        }

        sorted
    }
}
