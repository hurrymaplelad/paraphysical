import { LineContext, runtimeError } from "./errors.ts";
import {
  Expression,
  ParsedFile,
  parseFile,
  RefinedStatement,
} from "./parser.ts";

const LOCAL_DELIMITER = ":" as const;

export class Interpreter {
  #files: Map<string, ParsedFile>;
  // Key is filename:variablen. Use getLocal() / setLocal()
  #locals: Map<string, number>;
  #currentFilename = "";

  constructor() {
    this.#files = new Map();
    this.#locals = new Map();
  }

  load(filename: string, contents: string): void {
    this.#files.set(filename, parseFile(contents, { filename }));
  }

  run(filename: string): void {
    let programCounter = 0;
    const file = this.#files.get(filename);
    if (file == null) {
      throw new Error(`Cannot run "${filename}" - not loaded.`);
    }
    this.#currentFilename = filename;
    while (programCounter < file.maxLabel) {
      const statement = file.statements.get(programCounter);
      switch (statement?.type) {
        case "call":
          this.evaluateCall(statement);
          break;
      }
      programCounter += 1;
    }
  }

  evaluateCall(statement: RefinedStatement<"call">): void {
    const { functionName, args } = statement;
    switch (functionName.toUpperCase()) {
      case "LOCAL":
        return this.evaluateLOCAL(args, statement);
    }
    throw runtimeError(`unrecognized function: ${functionName}`, statement);
  }

  evaluateLOCAL(args: readonly Expression[], context: LineContext): void {
    for (const arg of args) {
      if (arg.type !== "reference") {
        throw runtimeError(`invalid LOCAL declaration: ${arg.type}`, context);
      }
      const name = arg.identifier;
      const key = this.#keyLocal(name);
      if (this.#isLocalDeclared(key)) {
        throw runtimeError(`attemped to re-declare LOCAL: ${name}`, context);
      }
      this.#locals.set(key, 0);
    }
  }

  #keyLocal(nameOrKey: string, filename: string | null = null): string {
    return nameOrKey.indexOf(LOCAL_DELIMITER) > -1
      ? nameOrKey
      : [filename ?? this.#currentFilename, nameOrKey].join(LOCAL_DELIMITER);
  }

  #isLocalDeclared(key: string): boolean {
    return this.#locals.has(key);
  }

  getLocal(nameOrKey: string, context: LineContext): number {
    const key = this.#keyLocal(nameOrKey);

    const value = this.#locals.get(key);
    if (value == null) {
      throw runtimeError(`attempted to read undeclared LOCAL: ${key}`, context);
    }
    return value;
  }

  setLocal(nameOrKey: string, value: number, context: LineContext): void {
    const key = this.#keyLocal(nameOrKey);
    if (!this.#isLocalDeclared(key)) {
      throw runtimeError(`attempted to set undeclared LOCAL: ${key}`, context);
    }
    this.#locals.set(key, value);
  }
}
