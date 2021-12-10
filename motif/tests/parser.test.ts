import { expect } from "chai";
import { parseImage, parseText, Program } from "../src/parser";
import { Checker, CheckerIrregular, PatternType, Rainbow, RainbowIrregular, Solid, Wave, WaveIrregular } from "../src/pattern";

describe("Parser test", () => {
  it ("Parses empty program", () => {
    let program = parseText("");
    expect(program).to.null;

    program = parseText("\n\n\n");
    expect(program).to.null;

    program = parseText("this is a comment because no pattern should be recognized\n\n");
    expect(program).to.null;

    // palette only without main section
    program = parseText("aabbcc\nddeeff");
    expect(program).to.null;
  });

  it ("Parses program with palette", () => {
    let a = 0; let b = 1; let c = 2;
    let d = 3; let e = 4; let f = 5;

    let source = [
      "",
      "this is a comment",
      "aabbcc",
      "aabbcc",
      "ghi", // comment because wrong width
      "ddeeff",
      "aaaaaa",
      "aaaaaa",
      "aaaaaa",
      "", // skipped
      "abc", // comment because wrong width
      "cccccc",  // c
      "cccccc",
      "cccccc",
      "ccbbaa",  // c b a, 2
      "ccbbaa",
      "bbccbb",  // b c, 2
      "bbccbb",
      "bbccbb",
      "bbccbb",
      "bbccbb",
      "abcbab",  // a b c, 1
      "aaabcc",  // a b c, 3 1 2
      "this is another comment",
      "deeded",  // d e, 1 2 1 1 1
      "deffed",  // d e f, 1 1 2 1 1
    ].join("\n");

    let program = parseText(source) as Program;

    expect(program).to.not.null;
    expect(program.palette.size).to.eq(6);

    const patterns = program.patterns;
    expect(patterns.length).to.eq(8);
    expect(patterns[0]).to.deep.eq(new Solid(7, a));
    expect(patterns[1]).to.deep.eq(new Solid(12, c));
    expect(patterns[2]).to.deep.eq(new Rainbow(15, [c, b, a], 2));
    expect(patterns[3]).to.deep.eq(new Checker(17, [b, c], 2));
    expect(patterns[4]).to.deep.eq(new Wave(22, [a, b, c], 1));
    expect(patterns[5]).to.deep.eq(new RainbowIrregular(23, [a, b, c], [3, 1, 2]));
    expect(patterns[6]).to.deep.eq(new CheckerIrregular(25, [d, e], [1, 2, 1, 1, 1]));
    expect(patterns[7]).to.deep.eq(new WaveIrregular(26, [d, e, f], [1, 1, 2, 1, 1]));
  });

  it ("Parses image data", () => {
    let a = 0xff0000ff;
    let b = 0xff00ff00;
    let c = 0xffff0000;

    let width = 6;
    let imgdata = new Uint32Array([
      a,b,c,a,b,c,
      a,a,a,a,a,a,
      a,a,b,b,a,a,
      b,a,c,a,b,a
    ])

    let height = imgdata.length / width;

    let program = parseImage(width, height, imgdata) as Program;
    expect(program).to.not.null;
    expect(program.palette.size).to.eq(3);

    const patterns = program.patterns;
    expect(patterns.length).to.eq(3);
    expect(patterns[0]).to.deep.eq(new Solid(2, 0));
    expect(patterns[1]).to.deep.eq(new Checker(3, [0, 1], 2));
    expect(patterns[2]).to.deep.eq(new Wave(4, [1, 0, 2], 1));
  });

  it("Parses text with UTF-8 characters", () => {
    let source = [
      "aa猫猫😋😋👍🏿👍🏿👍🏻👍🏻",
      "aaaaaaaaaa",
      "aa猫猫aa猫猫aa",
      "😋👍🏿👍🏻👍🏿😋👍🏿👍🏻👍🏿😋👍🏿"
    ].join("\n");

    let program = parseText(source) as Program;
    expect(program).to.not.null;
    expect(program.palette.size).to.eq(5);

    const patterns = program.patterns;
    expect(patterns.length).to.eq(3);
    expect(patterns[0]).to.deep.eq(new Solid(2, 0));
    expect(patterns[1]).to.deep.eq(new Checker(3, [0, 1], 2));
    expect(patterns[2]).to.deep.eq(new Wave(4, [2, 3, 4], 1));
  });

  it ("Report palette errors", () => {
    let source = "aaaaaa\nbbccbb\nccbbaa";
    expect(parseText.bind(null, source)).to.throw(/Palette is empty/);

    source = "aabbcc\nccddee\naaaaaa\naabbcc";
    expect(parseText.bind(null, source)).to.throw(/is already declared .* c/);

    source = "aabbcc\ndddddd\naabbcc";
    expect(parseText.bind(null, source)).to.throw(/is not declared .* d/);
  });
})
