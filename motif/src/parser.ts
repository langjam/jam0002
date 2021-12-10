import { Checker, CheckerIrregular, Pattern, PatternType, Rainbow, RainbowIrregular, Solid, Wave, WaveIrregular } from "./pattern";
import GraphemeSplitter from "grapheme-splitter";

const splitter = new GraphemeSplitter();

export type Glyph = number | string;
export type Palette = Map<Glyph, number>;
export type Program = {palette: Palette, patterns: Pattern[]};

export class ParseError extends Error {
  constructor(public line: number, public message: string) {
    super(message);
  }
}

export function parseText(text: string): Program | null {
  const lines = text.split(/\r?\n/g);
  return parse(lines.length, (i) => splitter.splitGraphemes(lines[i]));
}

export function parseImage(width: number, height: number, pixels: Uint32Array): Program | null {
  return parse(height, (lineIdx: number) => {
    let start = lineIdx * width;
    return Array.from(pixels.slice(start, start + width));
  })
}

type GetLineFunc = (index: number) => Glyph[];

function parse(numLines: number, getLine: GetLineFunc): Program | null {
  // parse color palette & width until first section marker
  let startIdx = 0;
  let width = 0;
  let lastMatch: MatchResult | null = null;

  const palette: Palette = new Map();

  do {
    const glyphs = getLine(startIdx++);
    if (width > 0 && glyphs.length !== width) continue;

    const match = matchPattern(startIdx, glyphs);
    if (match && !isMatchEqual(lastMatch, match)) {
      width = glyphs.length;
      lastMatch = match;

      if (match.type === PatternType.SOLID) {
        break;
      }

      addToPalette(palette, match);
    }
  } while(startIdx < numLines)

  if (!lastMatch || lastMatch.type !== PatternType.SOLID) return null;
  if (palette.size === 0) throw new ParseError(lastMatch.line, "Palette is empty");

  // parse the rest of program
  let patterns: Pattern[] = [ buildPattern(palette, lastMatch) ];
  for (let i = startIdx; i < numLines; i++) {
    const glyphs = getLine(i);
    if (glyphs.length !== width) continue;

    const match = matchPattern(i+1, glyphs);
    if (match && !isMatchEqual(lastMatch, match)) {
      patterns.push(buildPattern(palette, match));
      lastMatch = match;
    }
  }

  return {palette, patterns};
}

function buildPattern(palette: Palette, match: MatchResult): Pattern {
  const colors = match.symbols.map((symbol) => {
    let color = palette.get(symbol);
    if (color == null) throw new ParseError(match.line, `Symbol is not declared in palette: ${stringifyGlyph(symbol)}`);
    return color;
  });

  switch (match.type) {
    case PatternType.SOLID:
      return new Solid(match.line, colors[0]);
    case PatternType.RAINBOW:
      return new Rainbow(match.line, colors, match.sizes[0]);
    case PatternType.CHECKER:
      return new Checker(match.line, colors, match.sizes[0]);
    case PatternType.WAVE:
      return new Wave(match.line, colors, match.sizes[0]);
    case PatternType.RAINBOW_IRREGULAR:
      return new RainbowIrregular(match.line, colors, match.sizes);
    case PatternType.CHECKER_IRREGULAR:
      return new CheckerIrregular(match.line, colors, match.sizes);
    case PatternType.WAVE_IRREGULAR:
      return new WaveIrregular(match.line, colors, match.sizes);
  }
}

function addToPalette(palette: Palette, match: MatchResult) {
  for (let symbol of match.symbols) {
    if (palette.has(symbol)) throw new ParseError(match.line, `Symbol is already declared in palette: ${stringifyGlyph(symbol)}`)
    palette.set(symbol, palette.size);
  }
}

interface MatchResult {
  line: number;
  type: PatternType;
  symbols: Glyph[];
  sizes: number[];
}

export function stringifyGlyph(glyph: Glyph): string {
  if (typeof glyph === "string")
    return glyph;

  return glyph.toString(16);
}

function matchPattern(line: number, glyphs: Glyph[]): MatchResult | null {
  if (glyphs.length === 0) return null;

  let symbolIds = new Map<Glyph, number>();
  let previous: number = -1;
  let occurences: number[] = [];
  let sizes: number[] = [];

  for (let glyph of glyphs) {
    let id = symbolIds.get(glyph);
    if (id == null) {
      id = symbolIds.size;
      symbolIds.set(glyph, id);
    }

    if (previous === id) {
      sizes[sizes.length - 1] += 1;
      continue;
    }

    occurences.push(id);
    sizes.push(1);
    previous = id;
  }

  const symbols = Array.from(symbolIds.keys());

  if (symbols.length === 1) {
    return {line, type: PatternType.SOLID, symbols, sizes};
  }

  const uniform = (new Set(sizes)).size === 1;

  const increasingPattern = matchIncreasingPattern(occurences, symbols.length);
  if (increasingPattern === 1) {
    let type = uniform ? PatternType.RAINBOW : PatternType.RAINBOW_IRREGULAR;
    return {line, type, symbols, sizes};
  } else if (increasingPattern > 1) {
    let type = uniform ? PatternType.CHECKER : PatternType.CHECKER_IRREGULAR;
    return {line, type, symbols, sizes};
  }

  if (matchWavePattern(occurences, symbols.length)) {
    let type = uniform ? PatternType.WAVE : PatternType.WAVE_IRREGULAR;
    return {line, type, symbols, sizes};
  }

  return null;
}

function matchIncreasingPattern(occurences: number[], numSymbols: number): number {
  let repetition = 0;
  let expected = 0;

  for (let id of occurences) {
    if (id !== expected) return 0;
    if (id === 0) repetition++;

    expected = (expected + 1) % numSymbols;
  }

  return repetition;
}

function matchWavePattern(occurences: number[], numSymbols: number): boolean {
  const maxId = numSymbols - 1;
  let expected = 0;
  let ascending = true;

  for (let id of occurences) {
    if (id !== expected) return false;

    if (id === maxId) ascending = false;
    else if (id === 0) ascending = true;

    expected += ascending ? 1 : -1;
  }

  return true;
}

function isMatchEqual(a: MatchResult | null, b: MatchResult | null): boolean {
  if ((a == null) || (b == null) || (a.type !== b.type) || (a.symbols.length !== b.symbols.length) ||
      a.sizes.length !== b.sizes.length) {
    return false;
  }

  for (let i = 0; i < a.symbols.length; i++) {
    if (a.symbols[i] !== b.symbols[i]) {
      return false;
    }
  }

  for (let i = 0; i < a.sizes.length; i++) {
    if (a.sizes[i] !== b.sizes[i]) {
      return false;
    }
  }

  return true;
}
