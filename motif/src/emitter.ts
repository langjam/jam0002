import { Palette, Program } from "./parser";
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

interface EmitterState {
  currentSection: Section;
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
    currentSection: mainSection
  };

  for (let i = 1; i < patterns.length; i += 2) {
    // TODO: handle solid pattern
    let first = patterns[i];
    let second = patterns[i + 1];
    if (second == null) break;

    if (first.type === second.type) continue;

    let rule = CombinationTable[combine(first.type, second.type)];
    if (!rule) continue;
    else if (rule instanceof Function) {
      rule(state, first, second);
    } else {
      state.currentSection.bytecodes.push(rule);
    }
  }

  return {sections, mainSection, palette}
}

function combine(a: PatternType, b: PatternType): number {
  return a << 3 | b;
}

function sumColors(pattern: Pattern) {
  switch(pattern.type) {
    case PatternType.SOLID:
      return pattern.color;
    default:
      return pattern.colors.reduce((sum, color) => sum + color);
  }
}

type EmitFunction = (state: EmitterState, first: Pattern, second: Pattern) => void;

const CombinationTable: {[key in number]: Instructions | EmitFunction } = {
  [combine(PatternType.RAINBOW, PatternType.CHECKER)]           : emitPush,
  [combine(PatternType.CHECKER, PatternType.RAINBOW)]           : Instructions.POP,
  [combine(PatternType.RAINBOW, PatternType.WAVE_IRREGULAR)]    : Instructions.DUP,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.RAINBOW)]    : Instructions.SWAP,

  [combine(PatternType.RAINBOW, PatternType.WAVE)]      : Instructions.ADD,
  [combine(PatternType.WAVE, PatternType.RAINBOW)]      : Instructions.SUB,
  [combine(PatternType.CHECKER, PatternType.WAVE)]      : Instructions.MUL,
  [combine(PatternType.WAVE, PatternType.CHECKER)]      : Instructions.DIV,

  [combine(PatternType.RAINBOW, PatternType.RAINBOW_IRREGULAR)]      : Instructions.MOD,
  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.RAINBOW)]      : Instructions.POW,

  [combine(PatternType.RAINBOW_IRREGULAR, PatternType.WAVE_IRREGULAR)]    : Instructions.PRINT_INT,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.RAINBOW_IRREGULAR)]    : Instructions.PRINT_SYMB,

  [combine(PatternType.CHECKER_IRREGULAR, PatternType.WAVE_IRREGULAR)]    : Instructions.PRINT_CHAR,
  [combine(PatternType.WAVE_IRREGULAR, PatternType.CHECKER_IRREGULAR)]    : Instructions.HALT,

};

function emitPush(state: EmitterState, first: Pattern, second: Pattern) {
  state.currentSection.bytecodes.push(
    Instructions.PUSH,
    sumColors(second) - sumColors(first)
  );
}
