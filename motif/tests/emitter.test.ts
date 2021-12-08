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
  it ("Emit stack instructions", () => {
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
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.PUSH, 2, Instructions.POP, Instructions.DUP, Instructions.SWAP];

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

  it ("Emit print & halt instructions", () => {
    const lines = [
      "abcdefgh",
      "aaaaaaaa",
      // rainbow_irr-wave_irr: print int
      "aabbbbbb",
      "abccccba",
      // wave_irr-rainbow_irr: print symbol
      "abddddba",
      "aabbbbbb",

      // checker_irr-wave_irr: print char
      "aabbbaab",
      "abccccba",

      // wave_irr-checker_irr: halt
      "abddddba",
      "aabbbaab",
    ].join("\n");

    let program = runParseEmit(lines);

    let bytecodes = program.mainSection.bytecodes;
    let expected = [Instructions.PRINT_INT, Instructions.PRINT_SYMB, Instructions.PRINT_CHAR, Instructions.HALT];

    expect(bytecodes).to.have.length(expected.length);
    expect(bytecodes).to.deep.eq(expected);
  });
})
