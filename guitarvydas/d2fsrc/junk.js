var fname = "/Users/tarvydas/.local/bin/d2fsupport.js";

const fs = require ('fs');

console.log (fname);

var s = fs.readFileSync (fname, 'utf-8');

console.log (s);

var r = require (fname);


