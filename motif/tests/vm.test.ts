import { expect } from "chai";
import { CompiledProgram, Section } from "../src/emitter";
import { Palette } from "../src/parser";
import { Instructions, VM } from "../src/vm";

function createPrint(): [(str:string) => void, string[]] {
  let results: string[] = [];
  let print = (str: string) => { results.push(str); };

  return [print, results];
}

function joinPrintResult(results: string[]) {
  return ''.concat(...results);
}

function singleSectionProgram(bytecodes: number[], palette?: Palette): CompiledProgram {
  let mainSection = {color: 0, bytecodes}
  let program = {sections: new Map(), mainSection, palette: palette || new Map()};
  return program;
}

describe("VM test", () => {
  it('Run stack & memory instructions', () => {
    let program = singleSectionProgram([
      Instructions.PUSH, 2,
      Instructions.DUP,
      Instructions.PRINT_INT,
      Instructions.PRINT_INT,
      Instructions.PUSH, 3,
      Instructions.PUSH, 5,
      Instructions.SWAP,
      Instructions.PRINT_INT,
      Instructions.PRINT_INT,
      Instructions.PUSH, 8,
      Instructions.PUSH, 1,
      Instructions.STORE,
      Instructions.PUSH, 1,
      Instructions.LOAD,
      Instructions.PRINT_INT,
    ]);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("22358");
  });

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
    ]);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("516329");
  });

  it('Run logic & comparison instructions', () => {
    let program = singleSectionProgram([
      Instructions.PUSH, 2,
      Instructions.PUSH, 1,
      Instructions.GREATER,
      Instructions.PRINT_INT,

      Instructions.PUSH, 2,
      Instructions.PUSH, 1,
      Instructions.LESS,
      Instructions.PRINT_INT,

      Instructions.PUSH, 3,
      Instructions.PUSH, 3,
      Instructions.EQUAL,
      Instructions.PRINT_INT,

      Instructions.PUSH, 3,
      Instructions.NOT,
      Instructions.PRINT_INT,

      Instructions.PUSH, 2,
      Instructions.PUSH, 0,
      Instructions.AND,
      Instructions.PRINT_INT,


      Instructions.PUSH, 2,
      Instructions.PUSH, 0,
      Instructions.OR,
      Instructions.PRINT_INT,
    ]);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("101001");
  });

  it('Run control flow instructions', () => {
    let bytecodes = [
      Instructions.PUSH, 5,

      // block
      Instructions.DUP,
      Instructions.NOT,
      Instructions.JUMP_IF, 13,
      Instructions.DUP,
      Instructions.PRINT_INT,
      Instructions.PUSH, 1,
      Instructions.SUB,

      Instructions.JUMP, 2
      // end block
    ];

    let program = singleSectionProgram(bytecodes);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("54321");
  });

  it('Run print & halt instructions', () => {
    let palette: Palette = new Map([["a", 0], ["b", 1]]);
    let program = singleSectionProgram([
      Instructions.PUSH, 2,
      Instructions.PRINT_INT,
      Instructions.PUSH, 0,
      Instructions.PRINT_SYMB,
      Instructions.PUSH, 1,
      Instructions.PRINT_SYMB,
      Instructions.PUSH, 65,
      Instructions.PRINT_CHAR,
      Instructions.HALT,
      Instructions.PUSH, 65,
      Instructions.PRINT_CHAR,
    ], palette);

    let [print, results] = createPrint();
    new VM(program, print).run();

    expect(joinPrintResult(results)).to.eq("2abA");
  });
})
