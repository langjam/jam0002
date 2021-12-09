import { ParseError, parseText } from "./parser";
import fs from "fs/promises";
import { emitBytecodes } from "./emitter";
import { RuntimeError, VM } from "./vm";

async function run() {
  const filepath = process.argv[2];
  if (!filepath) {
    console.error("Usage: motif [filename]");
    return;
  }

  try {
    const codeFile = await fs.readFile(filepath);

    const program = parseText(codeFile.toString("utf8"));
    if (!program) return;

    const compiled = emitBytecodes(program);

    const vm = new VM(compiled, print);
    vm.run();
  } catch(e: any) {
    if (e instanceof ParseError) {
      console.error(`Parse error [line ${e.line}]:`, e.message);
    } else if (e instanceof RuntimeError) {
      console.error("Runtime error ", e.message);
    } else {
      console.error(e);
    }
  }


}

function print(str: string) {
  process.stdout.write(str);
}

run();
