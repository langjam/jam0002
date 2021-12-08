import { expect } from "chai";
import { emitBytecodes } from "../src/emitter";
import { parseText, Program } from "../src/parser"
import { Instructions } from "../src/vm";

function runParseEmit(text: string) {
  let program = parseText(text);
  expect(program).to.not.null;
  return emitBytecodes(program as Program);
}

describe("Emitter test", () => {
  it ("Emit stack & memory instructions", () => {
    const lines = [
      "abcdefgh",
      "aaaaaaaa",
      // rainbow-checker: push
      "aaaabbbb",
      "abcabcab",
      // checker-rainbow: pop
      "abababab",
      "aaaabbbb",
      // rainbow-wave_irreg: dup
      "bbbbaaaa",
      "abccbabc",
      // wave_irreg-rainbow: swap
      "abccccba",
      "aaaabbbb",
      // rainbow-checker_irreg: load
      "bbbbaaaa",
      "abbabbab",
      // checker_irreg-rainbow: store
      "abbbabbb",
      "aaaabbbb",
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.PUSH, 2, Instructions.POP, Instructions.DUP, Instructions.SWAP,
      Instructions.LOAD, Instructions.STORE];

    expect(bytecodes).to.have.length(expected.length);
    expect(bytecodes).to.deep.eq(expected);
  });

  it ("Emit arithmetic instructions", () => {
    const lines = [
      "abcdefgh",
      "aaaaaaaa",
      // rainbow-wave: add
      "aaaabbbb",
      "abcbabcb",
      // wave-rainbow: sub
      "abdbabdb",
      "aaaabbbb",
      // checker-wave: mul
      "abababab",
      "abcbabcb",
      // wave-checker: div
      "abdbabdb",
      "abababab",
      // rainbow-rainbow_irr: mod
      "aaaabbbb",
      "aaaaaabb",
      // rainbow_irr-rainbow: pow
      "aabbbbbb",
      "aaaabbbb",
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.ADD, Instructions.SUB, Instructions.MUL, Instructions.DIV,
      Instructions.MOD, Instructions.POW];

    expect(bytecodes).to.have.length(expected.length);
    expect(bytecodes).to.deep.eq(expected);
  });

  it ("Emit logic & comparison instructions", () => {
    const lines = [
      "abcdefgh",
      "aaaaaaaa",
      // checker-checker_irreg: greater
      "abababab",
      "abbbabbb",
      // checker-checker_irreg: less
      "cbbbcbbb",
      "abababab",
      // checker-rainbow_irreg: equal
      "acacacac",
      "aaaaaabb",
      // rainbow_irreg-checker: not
      "aaaaaacc",
      "abababab",
      // wave-rainbow_irreg: and
      "abcbabcb",
      "aaaaaacc",
      // rainbow_irreg-wave: or
      "aaaaaabb",
      "abcbabcb",
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.GREATER, Instructions.LESS, Instructions.EQUAL, Instructions.NOT,
      Instructions.AND, Instructions.OR];

    expect(bytecodes).to.have.length(expected.length);
    expect(bytecodes).to.deep.eq(expected);
  });

  it ("Emit print & halt instructions", () => {
    const lines = [
      "abcdefgh",
      "aaaaaaaa",
      // rainbow_irr-wave_irr: print int
      "aabbbbbb",
      "abccccba",
      // wave_irr-rainbow_irr: halt
      "abddddba",
      "aabbbbbb",

      // checker_irr-wave_irr: print char
      "aabbbaab",
      "abccccba",

      // wave_irr-checker_irr: print symbol
      "abddddba",
      "aabbbaab",
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.PRINT_INT, Instructions.HALT, Instructions.PRINT_CHAR, Instructions.PRINT_SYMB];

    expect(bytecodes).to.have.length(expected.length);
    expect(bytecodes).to.deep.eq(expected);
  });
})
