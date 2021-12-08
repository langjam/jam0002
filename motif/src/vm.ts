import { CompiledProgram, Section } from "./emitter";

export const enum Instructions {
  NOP,
  PUSH, POP, DUP, SWAP,
  ADD, SUB, MUL, DIV, MOD, POW,
  LOAD, STORE, GREATER, LESS,
  EQUAL, NOT, OR, AND,
  FWD, BACK, FWD_IF, BACK_IF,
  CALL, RETURN, PRINT_INT, PRINT_SYMB,
  PRINT_CHAR, HALT
};

export class RuntimeError extends Error {}
export type PrintFunc = (str: string) => void;

const STACK_SIZE = 1024;
const MEMORY_SIZE = 65536;

export class VM {
  private ip: number;
  private sp: number;
  private stack: Int32Array;
  private memory: Int32Array;
  private sectionStack: Section[];
  private symbols: string[]

  constructor(private program: CompiledProgram, private print: PrintFunc) {
    this.ip = 0;
    this.sp = 0;
    this.stack = new Int32Array(STACK_SIZE);
    this.memory = new Int32Array(MEMORY_SIZE)
    this.sectionStack = [ program.mainSection ];
    this.symbols = Array.from(program.palette.keys());
  }

  run() {
    this.ip = 0;
    for(;;) {
      let bytecodes = this.currentSection().bytecodes;

      if (this.ip < 0 || this.ip >= this.currentSection().bytecodes.length) return;

      let instr = this.currentSection().bytecodes[this.ip++] as Instructions;

      switch(instr) {
        /* STACK OPERATIONS */

        case Instructions.PUSH:
          this.push(bytecodes[this.ip++]);
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

        /* PRINTS */

        case Instructions.PRINT_INT:
          this.print(this.pop().toString());
        break;

        case Instructions.PRINT_SYMB: {
          let color = this.pop();
          if (color < 0 || color >= this.symbols.length) {
            throw new RuntimeError(`No symbol is associated to color #${color}`);
          }
          this.print(this.symbols[color]);
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
