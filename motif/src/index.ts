import { parseText } from "./parser";
import fs from "fs/promises";
import { emitBytecodes } from "./emitter";
import { VM } from "./vm";

async function run() {
  const filepath = process.argv[2];
  if (!filepath) {
    console.error("Usage: node dist/motif.js [filename]");
    return;
  }

  const codeFile = await fs.readFile(filepath);

  const program = parseText(codeFile.toString("utf8"));
  if (!program) return;

  const compiled = emitBytecodes(program);

  const vm = new VM(compiled, print);
  vm.run();
}

function print(str: string) {
  process.stdout.write(str);
}

run();
