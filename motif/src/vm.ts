import { CompiledProgram, Section } from "./emitter";
import { Glyph, stringifyGlyph } from "./parser";

export enum Instructions {
  NOP,
  PUSH, POP, DUP, SWAP,
  ADD, SUB, MUL, DIV, MOD, POW,
  LOAD, STORE, GREATER, LESS,
  EQUAL, NOT, OR, AND,
  JUMP, JUMP_IF, CALL, RETURN, HALT,
  PRINT_INT, PRINT_CHAR, PRINT_SYMB,
};

export class RuntimeError extends Error {}
export type PrintFunc = (str: string) => void;

const MAX_CALL_STACK = 512;
const STACK_SIZE = 1024;
const MEMORY_SIZE = 65536;

export class VM {
  private sp: number;
  private stack: Int32Array;
  private memory: Int32Array;
  private sectionStack: Section[];
  private returnStack: number[];
  private symbols: Glyph[]

  constructor(private program: CompiledProgram, private print: PrintFunc) {
    this.sp = 0;
    this.stack = new Int32Array(STACK_SIZE);
    this.memory = new Int32Array(MEMORY_SIZE)
    this.sectionStack = [ program.mainSection ];
    this.returnStack = [];
    this.symbols = Array.from(program.palette.keys());
  }

  run() {
    let ip = 0;
    for(;;) {
      const bytecodes = this.currentSection().bytecodes;

      if (ip < 0 || ip >= bytecodes.length) {
        this.sectionStack.pop();
        if (this.sectionStack.length === 0) return;
        ip = this.returnStack.pop() as number;
        continue;
      }

      let instr = bytecodes[ip++] as Instructions;

      switch(instr) {
        /* STACK OPERATIONS */

        case Instructions.PUSH:
          this.push(bytecodes[ip++]);
        break;
        case Instructions.POP:
          this.pop();
        break

        case Instructions.DUP:
          this.push(this.peek(1));
        break;
        case Instructions.SWAP:{
          let b = this.pop();
          let a = this.pop();
          this.push(b);
          this.push(a);
          break;
        }

        /* ARITHMETICS */

        case Instructions.ADD:
          this.push(this.pop() + this.pop());
        break;
        case Instructions.SUB:{
          let b = this.pop();
          let a = this.pop();

          this.push(a - b);
          break;
        }
        case Instructions.MUL:
          this.push(this.pop() * this.pop());
        break;
        case Instructions.DIV:{
          let b = this.pop();
          let a = this.pop();

          if (b === 0) throw new RuntimeError("Division by zero.");

          this.push(Math.floor(a / b));
          break;
        }

        case Instructions.MOD:{
          let b = this.pop();
          let a = this.pop();

          if (b === 0) throw new RuntimeError("Division by zero.");

          this.push(a % b);
          break;
        }
        case Instructions.POW:{
          let b = this.pop();
          let a = this.pop();

          this.push(a ** b);
          break;
        }

        /* LOGIC & COMPARISON */
        case Instructions.GREATER: {
          let b = this.pop();
          let a = this.pop();
          this.push(a > b ? 1 : 0);
          break;
        }

        case Instructions.LESS: {
          let b = this.pop();
          let a = this.pop();
          this.push(a < b ? 1 : 0);
          break;
        }

        case Instructions.EQUAL: {
          let b = this.pop();
          let a = this.pop();
          this.push(a === b ? 1 : 0);
          break;
        }

        case Instructions.NOT: {
          this.push((!this.pop()) ? 1 : 0);
          break;
        }

        case Instructions.AND: {
          let b = this.pop();
          let a = this.pop();
          this.push(a && b ? 1 : 0);
          break;
        }

        case Instructions.OR: {
          let b = this.pop();
          let a = this.pop();
          this.push(a || b ? 1 : 0);
          break;
        }

        /* MEMORY OPERATIONS */
        case Instructions.LOAD: {
          let addr = this.pop();
          if (addr < 0 || addr >= this.memory.length) {
            throw new RuntimeError("Memory out of bound");
          }

          this.push(this.memory[addr]);
          break;
        }

        case Instructions.STORE: {
          let addr = this.pop();
          if (addr < 0 || addr >= this.memory.length) {
            throw new RuntimeError("Memory out of bound");
          }

          this.memory[addr] = this.pop();
          break;
        }

        /* CONTROL FLOW */

        case Instructions.JUMP: {
          let next = bytecodes[ip++];
          ip = next;
          break;
        }

        case Instructions.JUMP_IF: {
          let next = bytecodes[ip++];
          let condition = this.pop();

          if (!!condition) {
            ip = next;
          }

          break;
        }

        case Instructions.RETURN:
          ip = -1;
        break;

        case Instructions.CALL: {
          let sectionColor = bytecodes[ip++];
          let section = this.program.sections.get(sectionColor);
          if (!section) throw new RuntimeError(`Section with color #${sectionColor} doesn't exists`);

          if (this.sectionStack.length === MAX_CALL_STACK) {
            throw new RuntimeError("Stack overflow");
          }

          this.sectionStack.push(section);
          this.returnStack.push(ip);
          ip = 0;

          break;
        }

        /* PRINTS */

        case Instructions.PRINT_INT:
          this.print(this.pop().toString());
        break;

        case Instructions.PRINT_SYMB: {
          let color = this.pop();
          if (color < 0 || color >= this.symbols.length) {
            throw new RuntimeError(`No symbol is associated to color #${color}`);
          }
          this.print(stringifyGlyph(this.symbols[color]));
          break;
        }

        case Instructions.PRINT_CHAR:
          this.print(String.fromCharCode(this.pop() & 0xFF));
        break;

        case Instructions.HALT:
          return;
      }
    }
  }

  private currentSection() {
    return this.sectionStack[this.sectionStack.length - 1] as Section;
  }

  private push(value: number) {
    if (this.sp >= this.stack.length) {
      throw new RuntimeError("Stack overflow.");
    }

    this.stack[this.sp++] = value;
  }

  private pop(): number {
    if (this.sp <= 0) {
      throw new RuntimeError("Stack underflow.");
    }

    return this.stack[--this.sp];
  }

  private peek(delta: number = 1){
    let pos = this.sp - delta;
    if (pos < 0) {
      throw new RuntimeError("Stack underflow");
    }

    return this.stack[pos];
  }
}

function debugBytecodes(section: Section) {
  const bytecodes = section.bytecodes;
  const result: string[] = [];

  let i = 0;
  while (i < bytecodes.length) {
    const instr = bytecodes[i++];
    let str;

    switch(instr) {
      case Instructions.PUSH:
      case Instructions.JUMP:
      case Instructions.JUMP_IF:
      case Instructions.CALL:
        str = `${i} ${Instructions[instr]} ${bytecodes[i++]}`;
      break;
      default:
        str = `${i} ${Instructions[instr]}`;
    }

    result.push(str);
  }

  console.log(result.join("\n"));
}
