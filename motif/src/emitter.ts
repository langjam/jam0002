import { Palette, ParseError, Program } from "./parser";
import { Pattern, PatternType, Solid } from "./pattern";
import { Instructions } from "./vm";

export interface Section {
  color: number;
  bytecodes: number[];
}

export interface CompiledProgram {
  sections: Map<number, Section>;
  mainSection: Section;
  palette: Palette;
}

interface BlockData {
  startPos: number,
  endCorrections: number[]
}

interface EmitterState {
  currentSection: Section;
  blocks: BlockData[]
}

export function emitBytecodes(program: Program): CompiledProgram {
  const {palette, patterns} = program;

  let firstPattern = patterns[0];
  if (!(firstPattern instanceof Solid)) {
    throw new Error("Unreachable: invalid first section");
  }

  const mainSection = {color: firstPattern.color, bytecodes: []};

  const sections = new Map<number, Section>();
  sections.set(mainSection.color, mainSection);

  let state: EmitterState = {
    currentSection: mainSection,
    blocks: []
  };

  for (let i = 1; i < patterns.length; i += 2) {
    // TODO: handle solid pattern
    let first = patterns[i];
    let second = patterns[i + 1];
    if (second == null) break;

    if (first.type === second.type) continue;

    const rule = CombinationTable[combine(first.type, second.type)];
    if (rule instanceof Function) {
      rule(state, first, second);
    } else if (rule != null) {
      state.currentSection.bytecodes.push(rule);
    }
  }

  return {sections, mainSection, palette}
}

function combine(a: PatternType, b: PatternType): number {
  return a << 3 | b;
}

function sumColors(pattern: Pattern): number {
  switch(pattern.type) {
    case PatternType.SOLID:
      return pattern.color;
    default:
      return pattern.colors.reduce((sum, color) => sum + color);
  }
}

function colorDifference(first: Pattern, second: Pattern): number {
  return sumColors(second) - sumColors(first);
}

type EmitFunction = (state: EmitterState, first: Pattern, second: Pattern) => void;

const CombinationTable: {[key in number]: Instructions | EmitFunction } = {
  [combine(PatternType.RAINBOW, PatternType.CHECKER)]           : emitPush,
  [combine(PatternType.CHECKER, PatternType.RAINBOW)]           : Instructions.POP,
  [combine(PatternType.RAINBOW, PatternType.WAVE_IRREGULAR)]    : Instructions.DUP,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.RAINBOW)]    : Instructions.SWAP,

  [combine(PatternType.RAINBOW, PatternType.WAVE)]                   : Instructions.ADD,
  [combine(PatternType.WAVE, PatternType.RAINBOW)]                   : Instructions.SUB,
  [combine(PatternType.CHECKER, PatternType.WAVE)]                   : Instructions.MUL,
  [combine(PatternType.WAVE, PatternType.CHECKER)]                   : Instructions.DIV,
  [combine(PatternType.RAINBOW, PatternType.RAINBOW_IRREGULAR)]      : Instructions.MOD,
  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.RAINBOW)]      : Instructions.POW,

  [combine(PatternType.RAINBOW, PatternType.CHECKER_IRREGULAR)]      : Instructions.LOAD,
  [combine(PatternType.CHECKER_IRREGULAR, PatternType.RAINBOW)]      : Instructions.STORE,

  [combine(PatternType.CHECKER, PatternType.CHECKER_IRREGULAR)]      : Instructions.GREATER,
  [combine(PatternType.CHECKER_IRREGULAR, PatternType.CHECKER)]      : Instructions.LESS,
  [combine(PatternType.CHECKER, PatternType.RAINBOW_IRREGULAR)]      : Instructions.EQUAL,
  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.CHECKER)]      : Instructions.NOT,
  [combine(PatternType.WAVE, PatternType.RAINBOW_IRREGULAR)]         : Instructions.AND,
  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.WAVE)]         : Instructions.OR,

  [combine(PatternType.WAVE, PatternType.WAVE_IRREGULAR)]            : startBlock,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.WAVE)]            : endBlock,
  [combine(PatternType.WAVE, PatternType.CHECKER_IRREGULAR)]         : emitFwd,
  [combine(PatternType.CHECKER_IRREGULAR, PatternType.WAVE)]         : emitBack,
  [combine(PatternType.CHECKER, PatternType.WAVE_IRREGULAR)]         : emitFwdIf,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.CHECKER)]         : emitBackIf,


  [combine(PatternType.WAVE_IRREGULAR, PatternType.RAINBOW_IRREGULAR)]    : Instructions.HALT,
  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.WAVE_IRREGULAR)]    : Instructions.PRINT_INT,
  [combine(PatternType.CHECKER_IRREGULAR, PatternType.WAVE_IRREGULAR)]    : Instructions.PRINT_CHAR,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.CHECKER_IRREGULAR)]    : Instructions.PRINT_SYMB,

};

function emitPush(state: EmitterState, first: Pattern, second: Pattern) {
  state.currentSection.bytecodes.push(
    Instructions.PUSH,
    colorDifference(first, second)
  );
}

function startBlock(state: EmitterState, first: Pattern, second: Pattern) {
  const startPos = state.currentSection.bytecodes.length;
  state.blocks.push({startPos, endCorrections: []});
}

function endBlock(state: EmitterState, first: Pattern, second: Pattern) {
  const block = state.blocks.pop();
  if (block == null) {
    throw new ParseError(first.line, "Mismatched number of start block & end block.");
  }

  const bytecodes = state.currentSection.bytecodes;
  for (let pos of block.endCorrections) {
    if (pos < 0 || pos >= bytecodes.length) {
      throw new Error("Unreachable: out of bound jump end position correction");
    }

    bytecodes[pos] = bytecodes.length;
  }
}

function emitFwd(state: EmitterState, first: Pattern, second: Pattern) {
  let target = colorDifference(first, second);
  target = state.blocks.length - target - 1;

  if (target < 0) {
    throw new ParseError(first.line, "Can't jump to outside of block");
  }

  const bytecodes = state.currentSection.bytecodes;
  bytecodes.push(Instructions.JUMP);
  state.blocks[target].endCorrections.push(bytecodes.length);
  bytecodes.push(0);
}

function emitBack(state: EmitterState, first: Pattern, second: Pattern) {
  let target = colorDifference(first, second);
  target = state.blocks.length - target - 1;

  if (target < 0) {
    throw new ParseError(first.line, "Can't jump to outside of block");
  }

  state.currentSection.bytecodes.push(Instructions.JUMP, state.blocks[target].startPos);
}

function emitFwdIf(state: EmitterState, first: Pattern, second: Pattern) {
  let target = colorDifference(first, second);
  target = state.blocks.length - target - 1;

  if (target < 0) {
    throw new ParseError(first.line, "Can't jump to outside of block");
  }

  const bytecodes = state.currentSection.bytecodes;
  bytecodes.push(Instructions.JUMP_IF);
  state.blocks[target].endCorrections.push(bytecodes.length);
  bytecodes.push(0);

}

function emitBackIf(state: EmitterState, first: Pattern, second: Pattern) {
  let target = colorDifference(first, second);
  target = state.blocks.length - target - 1;

  if (target < 0) {
    throw new ParseError(first.line, "Can't jump to outside of block");
  }

  state.currentSection.bytecodes.push(Instructions.JUMP_IF, state.blocks[target].startPos);
}
