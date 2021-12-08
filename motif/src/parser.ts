import { Checker, CheckerIrregular, Pattern, PatternType, Rainbow, RainbowIrregular, Solid, Wave, WaveIrregular } from "./pattern";

export type Palette = Map<string, number>;
export type Program = {palette: Palette, patterns: Pattern[]};
export class ParseError extends Error {}

export function parseText(text: string): Program | null {
  const lines = text.split(/\r?\n/g);

  // parse color palette & width until first section marker
  let startIdx = 0;
  let width = 0;
  let lastMatch: MatchResult | null = null;

  const palette: Palette = new Map();

  do {
    const line = lines[startIdx++];
    const chars = line.split("");
    if (width > 0 && chars.length !== width) continue;

    const match = matchPattern(chars);
    if (match && !isMatchEqual(lastMatch, match)) {
      width = chars.length;
      lastMatch = match;

      if (match.type === PatternType.SOLID) {
        break;
      }

      addToPalette(palette, match);
    }
  } while(startIdx < lines.length)

  if (!lastMatch || lastMatch.type !== PatternType.SOLID) return null;
  if (palette.size === 0) throw new ParseError("Palette is empty");

  // parse the rest of program
  let patterns: Pattern[] = [ buildPattern(palette, lastMatch) ];
  for (let i = startIdx; i < lines.length; i++) {
    const line = lines[i];
    const chars = line.split("");
    if (chars.length !== width) continue;

    const match = matchPattern(chars);
    if (match && !isMatchEqual(lastMatch, match)) {
      patterns.push(buildPattern(palette, match));
      lastMatch = match;
    }
  }

  return {palette, patterns};
}

function buildPattern(palette: Palette, match: MatchResult): Pattern {
  const colors = match.symbols.map((symbol) => getColor(palette, symbol));

  switch (match.type) {
    case PatternType.SOLID:
      return new Solid(colors[0]);
    case PatternType.RAINBOW:
      return new Rainbow(colors, match.sizes[0]);
    case PatternType.CHECKER:
      return new Checker(colors, match.sizes[0]);
    case PatternType.WAVE:
      return new Wave(colors, match.sizes[0]);
    case PatternType.RAINBOW_IRREGULAR:
      return new RainbowIrregular(colors, match.sizes);
    case PatternType.CHECKER_IRREGULAR:
      return new CheckerIrregular(colors, match.sizes);
    case PatternType.WAVE_IRREGULAR:
      return new WaveIrregular(colors, match.sizes);
  }
}

function addToPalette(palette: Palette, match: MatchResult) {
  for (let symbol of match.symbols) {
    if (palette.has(symbol)) throw new ParseError(`Symbol is already declared in palette: ${symbol}`)
    palette.set(symbol, palette.size);
  }
}

function getColor(palette: Palette, symbol: string): number {
  let color = palette.get(symbol);
  if (color == null) throw new ParseError(`Symbol is not declared in palette: ${symbol}`);
  return color;
}

interface MatchResult {
  type: PatternType;
  symbols: string[];
  sizes: number[];
}

function matchPattern(chars: string[]): MatchResult | null {
  if (chars.length === 0) return null;

  let symbolIds = new Map<string, number>();
  let previous: number = -1;
  let occurences: number[] = [];
  let sizes: number[] = [];

  for (let char of chars) {
    let id = symbolIds.get(char);
    if (id == null) {
      id = symbolIds.size;
      symbolIds.set(char, id);
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
    return {type: PatternType.SOLID, symbols, sizes};
  }

  const uniform = (new Set(sizes)).size === 1;

  const increasingPattern = matchIncreasingPattern(occurences, symbols.length);
  if (increasingPattern === 1) {
    let type = uniform ? PatternType.RAINBOW : PatternType.RAINBOW_IRREGULAR;
    return {type, symbols, sizes};
  } else if (increasingPattern > 1) {
    let type = uniform ? PatternType.CHECKER : PatternType.CHECKER_IRREGULAR;
    return {type, symbols, sizes};
  }

  if (matchWavePattern(occurences, symbols.length)) {
    let type = uniform ? PatternType.WAVE : PatternType.WAVE_IRREGULAR;
    return {type, symbols, sizes};
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
