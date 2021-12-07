import { expect } from "chai";
import { parseText, Program } from "../src/parser";
import { Checker, CheckerIrregular, PatternType, Rainbow, RainbowIrregular, Solid, Wave, WaveIrregular } from "../src/pattern";

describe("Parser test", () => {
  it ("Parses empty program", () => {
    let program = parseText("");
    expect(program).to.null;

    program = parseText("\n\n\n");
    expect(program).to.null;

    program = parseText("this is a comment because no pattern should be recognized\n\n");
    expect(program).to.null;
  });

  it ("Parses program with palette", () => {
    let a = 0;
    let b = 1;
    let c = 2;

    let source = [
      "aabbcc",
      "", // skipped
      "abc", // comment because wrong width
      "cccccc",  // c
      "ccbbaa",  // c b a, 2
      "bbccbb",  // b c, 2
      "abcbab",  // a b c, 1
      "aaabcc",  // a b c, 3 1 2
      "this is a comment",
      "bccbcb",  // b c, 1 2 1 1 1
      "abccba",  // a b c, 1 1 2 1 1
    ].join("\n");

    let program = parseText(source) as Program;

    expect(program).to.not.null;
    expect(program.palette.size).to.eq(3);

    const patterns = program.patterns;
    expect(patterns.length).to.eq(7);
    expect(patterns[0]).to.deep.eq(new Solid(c));
    expect(patterns[1]).to.deep.eq(new Rainbow([c, b, a], 2));
    expect(patterns[2]).to.deep.eq(new Checker([b, c], 2));
    expect(patterns[3]).to.deep.eq(new Wave([a, b, c], 1));
    expect(patterns[4]).to.deep.eq(new RainbowIrregular([a, b, c], [3, 1, 2]));
    expect(patterns[5]).to.deep.eq(new CheckerIrregular([b, c], [1, 2, 1, 1, 1]));
    expect(patterns[6]).to.deep.eq(new WaveIrregular([a, b, c], [1, 1, 2, 1, 1]));
  });

  it ("Parses program without palette", () => {
    let a = 97;
    let b = 98;
    let c = 99;

    let source = [
      "aaaaaa",
      "", // skipped
      "abc", // comment because wrong width
      "cccccc",  // c
      "ccbbaa",  // c b a, 2
      "bbccbb",  // b c, 2
      "abcbab",  // a b c, 1
      "aaabcc",  // a b c, 3 1 2
      "this is a comment",
      "bccbcb",  // b c, 1 2 1 1 1
      "abccba",  // a b c, 1 1 2 1 1
    ].join("\n");

    let program = parseText(source) as Program;

    expect(program).to.not.null;
    expect(program.palette.size).to.eq(0);

    const patterns = program.patterns;
    expect(patterns.length).to.eq(7);
    expect(patterns[0]).to.deep.eq(new Solid(c));
    expect(patterns[1]).to.deep.eq(new Rainbow([c, b, a], 2));
    expect(patterns[2]).to.deep.eq(new Checker([b, c], 2));
    expect(patterns[3]).to.deep.eq(new Wave([a, b, c], 1));
    expect(patterns[4]).to.deep.eq(new RainbowIrregular([a, b, c], [3, 1, 2]));
    expect(patterns[5]).to.deep.eq(new CheckerIrregular([b, c], [1, 2, 1, 1, 1]));
    expect(patterns[6]).to.deep.eq(new WaveIrregular([a, b, c], [1, 1, 2, 1, 1]));
  });
})
