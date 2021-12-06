import { PatternType } from "./pattern";

export function lexText(text: string) {
  const lines = text.split(/\r?\n/g);

  for (let line of lines) {
    if (line.length === 0) continue;
    const chars = line.split("");
    const match = matchPattern(chars);
    console.log(match);
  }
}

interface MatchResult {
  type: PatternType;
  symbols: string[];
  sizes: number[];
  length: number;
}

export function matchPattern(chars: string[]): MatchResult | null {
  let symbolIds = new Map<string, number>();

  let previous: number = -1;
  let previousChar: string | null = null;
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
    previousChar = char;
  }

  const symbols = Array.from(symbolIds.keys());
  const length = chars.length;

  if (symbols.length === 1) {
    return {type: PatternType.SOLID, symbols, sizes, length};
  }

  const uniform = (new Set(sizes)).size === 1;

  const increasingPattern = matchIncreasingPattern(occurences, symbols.length);
  if (increasingPattern === 1) {
    let type = uniform ? PatternType.RAINBOW : PatternType.RAINBOW_IRREGULAR;
    return {type, symbols, sizes, length};
  } else if (increasingPattern > 1) {
    let type = uniform ? PatternType.CHECKER : PatternType.CHECKER_IRREGULAR;
    return {type, symbols, sizes, length};
  }

  if (matchWavePattern(occurences, symbols.length)) {
    let type = uniform ? PatternType.WAVE : PatternType.WAVE_IRREGULAR;
    return {type, symbols, sizes, length};
  }

  return null;
}

function matchIncreasingPattern(occurences: number[], numSymbols: number) {
  let repetition = 0;
  let expected = 0;

  for (let id of occurences) {
    if (id !== expected) return 0;
    if (id === 0) repetition++;

    expected = (expected + 1) % numSymbols;
  }

  return repetition;
}

function matchWavePattern(occurences: number[], numSymbols: number) {
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
