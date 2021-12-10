# Daemon

Daemon is a domain-specific language that helps to decouple program state from temporal state. It achieves this via pattern matching, but instead of matching on values and types like in many other languages, it matches on the temporal relation of one event to another.

Daemon is both a language and an engine that handles temporal logic, much like regular expressions.

Daemon engine is very like a state machine, but unlike in a state machine different actions do not necessarily lead to different states. Instead the current state takes one form or another based on whether some events happened in the past (or are yet to happen).

Daemon is useful when there are a lot of optional and very similar states. A real state machine (and its code) would quickly become extremely uncomprehensible in such cases. The other alternative would be to maintain a big number of boolean flags right aside other important logic which is even worse. Daemon solves this problem by being less strict about the validity of a single state.

## Applications

Anywhere where the order of events can be optional and states can be valid regardless of whether some events happened or not in the past. Also it allows to colocate valid states together, unlike in a state machine where different states would be different nodes.

* Non-linear games
* Data processing flows
* Local development tools (see Docker Compose and its management of order of container loading)

## Feel

Here's what a Daemon source file looks like when used for some non-linear game (which you can try as an example!):

```
on check_inventory {
    anticipate all => check_inventory_first
}

on visit_village {
    after visit_village => nothing_to_do_here
}

on visit_market {
    before search_crates => useless_poison
    anticipate fight_monster => weapon_not_enough
}

on search_crates {
    anticipate fight_monster => weapon_in_crates
}

on fight_monster {
    after visit_market => apply_poison
    before visit_village => more_friends_to_meet
}
```

## Syntax

Generally, source files consist of one or more handlers of the following form:

```
on [event_name] {
    [before|after|anticipate] [another_event_name] => your_message
}
```

It reads as: on `event_name`, if it is `before` (or `after`) `another_event_name`, then produce `your_message`. `your_message` is a simple string that the engine gives back to you. What this message means is completely up to you. It can be logged immediately or used to invoke methods that change the program state (separate from temporal order).

`anticipate` is what helps you to make the order stricter. If an event A anticipates event B, then when event B occurs and it turns out to be before event A (= event A is not in history), Daemon produces the given message (and does not save event A in the history).

You can use a special keyword `all` in the place of the event name to ensure that that anticipating event always before every other event. This is the strictest possible setting. 

## Grammar

```
handler := on event-name body LINESPACE*
body := '{' (pattern LINESPACE )* '}'
pattern := temporal-rel event-name '=>' MESSAGE
temporal-rel := 'before' | 'after' | 'anticipate'
event-name := IDENTIFIER | 'all'

LINESPACE := (NEWLINE | WHITESPACE) NEWLINE*
NEWLINE := LF | CRLF
IDENTIFIER := ALPHA ( ALPHA | DIGIT | '_' )* ;
MESSAGE := ( '_' | ALPHA | DIGIT | WHITESPACE )*;
```

## Examples

The example shows how Daemon engine can be used in a relatively non-linear game. In this game, you are an explorer that tries to defeat the legendary monster that lives in the dungeon and guards a hidden treasure.
To run the game, do:

```
cargo r --example game -q
```

You will be presented a choice to make one of the allowed actions at a time and get output based on the logic defined in the `input.daemon` file. Note that some actions do not necessarily lead to text output.
