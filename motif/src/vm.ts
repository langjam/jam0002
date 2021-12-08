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

export class VM {
  private ip: number;
  private sp: number;
  private stack: Int32Array;
  private sectionStack: Section[];

  constructor(private program: CompiledProgram, private print: PrintFunc) {
    this.ip = 0;
    this.sp = 0;
    this.stack = new Int32Array(1064);
    this.sectionStack = [ program.mainSection ];
  }

  run() {
    this.ip = 0;
    for(;;) {
      let bytecodes = this.currentSection().bytecodes;

      if (this.ip < 0 || this.ip >= this.currentSection().bytecodes.length) return;

      let instr = this.currentSection().bytecodes[this.ip++] as Instructions;

      switch(instr) {
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

        case Instructions.PRINT_INT:
          this.print(this.pop().toString());
        break;
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