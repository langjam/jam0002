import { expect } from "chai";
import { CompiledProgram, Section } from "../src/emitter";
import { Instructions, VM } from "../src/vm";

function createPrint(): [(str:string) => void, string[]] {
  let results: string[] = [];
  let print = (str: string) => { results.push(str); };

  return [print, results];
}

function joinPrintResult(results: string[]) {
  return ''.concat(...results);
}

function singleSectionProgram(bytecodes: number[]): CompiledProgram {
  let mainSection = {color: 0, bytecodes}
  let program = {sections: new Map(), mainSection};
  return program;
}

describe("VM test", () => {
  it('Run arithmetic instructions', () => {
    let program = singleSectionProgram([
      Instructions.PUSH, 2,
      Instructions.PUSH, 3,
      Instructions.ADD,
      Instructions.PRINT_INT,
      Instructions.PUSH, 3,
      Instructions.PUSH, 2,
      Instructions.SUB,
      Instructions.PRINT_INT,
      Instructions.PUSH, 3,
      Instructions.PUSH, 2,
      Instructions.MUL,
      Instructions.PRINT_INT,
      Instructions.PUSH, 7,
      Instructions.PUSH, 2,
      Instructions.DIV,
      Instructions.PRINT_INT,
      Instructions.PUSH, 5,
      Instructions.PUSH, 3,
      Instructions.MOD,
      Instructions.PRINT_INT,
      Instructions.PUSH, 3,
      Instructions.PUSH, 2,
      Instructions.POW,
      Instructions.PRINT_INT,
      Instructions.PUSH, 2,
      Instructions.DUP,
      Instructions.PRINT_INT,
      Instructions.PRINT_INT,
      Instructions.PUSH, 3,
      Instructions.PUSH, 5,
      Instructions.SWAP,
      Instructions.PRINT_INT,
      Instructions.PRINT_INT,
    ]);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("5163292235");
  })
})
