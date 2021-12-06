export enum PatternType {
  SOLID,
  RAINBOW,
  CHECKER,
  WAVE,
  RAINBOW_IRREGULAR,
  CHECKER_IRREGULAR,
  WAVE_IRREGULAR,
}

export interface Pattern { type: PatternType }

export class Solid implements Pattern {
  readonly type = PatternType.SOLID;
  constructor(public color: number) {}
}

export class Rainbow implements Pattern {
  readonly type = PatternType.RAINBOW;
  constructor(public colors: number[], public cellSize: number){}
}

export class Checker implements Pattern {
  readonly type = PatternType.CHECKER;
  constructor(public colors: number[], public cellSize: number){}
}

export class Wave implements Pattern {
  readonly type = PatternType.WAVE;
  constructor(public colors: number[], public cellSize: number){}
}

export class RainbowIrregular implements Pattern {
  readonly type = PatternType.RAINBOW_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}

export class CheckerIrregular implements Pattern {
  readonly type = PatternType.CHECKER_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}

export class WaveIrregular implements Pattern {
  readonly type = PatternType.WAVE_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}
