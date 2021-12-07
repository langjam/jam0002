import { parseText } from "./parser";

const text =
`
111111

112233
112231
111123
123212
123321
111111
222222
123321
`
let patterns = parseText(text);
console.log(patterns?.palette);
console.log(patterns?.patterns);
