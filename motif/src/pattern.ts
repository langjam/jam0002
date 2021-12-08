export enum PatternType {
  SOLID,
  RAINBOW,
  CHECKER,
  WAVE,
  RAINBOW_IRREGULAR,
  CHECKER_IRREGULAR,
  WAVE_IRREGULAR,
}

export type Pattern = Solid | Rainbow | Checker | Wave | RainbowIrregular | CheckerIrregular | WaveIrregular;

export class Solid {
  readonly type = PatternType.SOLID;
  constructor(public color: number) {}
}

export class Rainbow {
  readonly type = PatternType.RAINBOW;
  constructor(public colors: number[], public cellSize: number){}
}

export class Checker {
  readonly type = PatternType.CHECKER;
  constructor(public colors: number[], public cellSize: number){}
}

export class Wave {
  readonly type = PatternType.WAVE;
  constructor(public colors: number[], public cellSize: number){}
}

export class RainbowIrregular {
  readonly type = PatternType.RAINBOW_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}

export class CheckerIrregular {
  readonly type = PatternType.CHECKER_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}

export class WaveIrregular {
  readonly type = PatternType.WAVE_IRREGULAR;
  constructor(public colors: number[], public cellSizes: number[]){}
}
