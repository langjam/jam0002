const chai = require("chai");
const expect = chai.expect;
const parser = require("../src/parser.js");

describe("Parser tests", () => {
    it("should properly parse input", () => {
        const parsed = parser.parseCode("[20 +] [2 -3 1 +]");
        expect(parsed).to.deep.equal({
            0: {
                commands: [20, "+"],
                repetitions: 1
            },
            1: {
                commands: [2, -3, 1, "+"],
                repetitions: 1
            }
        });
    });
    it("should properly allow optionally repetitions to be declared", () => {
        const parsed = parser.parseCode("[20 +]8 [2 -3 1 +]");
        expect(parsed).to.deep.equal({
            0: {
                commands: [20, "+"],
                repetitions: 8
            },
            1: {
                commands: [2, -3, 1, "+"],
                repetitions: 1
            }
        });
    });
    it("should properly parse code blocks in groups", () => {
        const parsed = parser.parseCode("[20 +] [2 -3 1 +] [(1 1 1 +) (2 2 2 +) 2 x > if]");
        expect(parsed).to.deep.equal({
            0: {
                commands: [20, "+"],
                repetitions: 1
            },
            1: {
                commands: [2, -3, 1, "+"],
                repetitions: 1
            },
            2: {
                commands: [
                    [1, 1, 1, "+"],
                    [2, 2, 2, "+"],
                    2, "x", ">", "if"],
                repetitions: 1
            }
        });
    });
});