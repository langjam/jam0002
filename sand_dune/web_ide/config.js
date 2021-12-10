
// Dynamic Highlighting attempts to respect in-file syntax extensions.
// It's janky and slow
//  (it's implemented by, every few seconds, re-parsing the file
//    (which involves running phase-1+ code),
//    and finding long lines in the syntax extension
//     to mark where the language changes),
//  but it makes for a cool demo!
dynamic_highlighting = false;

// Start by editing this file:
starter_file = "rule_110_glider.sand_dune";

// If empty, Unseemly. Otherwise, the URL of the language to use
//  for execution and syntax-highlighting purposes.
base_language = "sand_dune.unseemly";
