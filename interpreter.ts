import { LineContext, runtimeError } from "./errors.ts";
import {
  Expression,
  ParsedFile,
  parseFile,
  parseReferenceIdentifier,
  RefinedStatement,
} from "./parser.ts";

const LOCAL_DELIMITER = ":" as const;

export class Interpreter {
  #files: Map<string, ParsedFile>;
  // Key is filename:variablen. Use getLocal() / setLocal()
  #locals: Map<string, number>;
  #points: Map<string, number>;
  #currentFilename = "";

  constructor() {
    this.#files = new Map();
    this.#locals = new Map();
    this.#points = new Map();
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
    while (programCounter <= file.maxLabel) {
      const statement = file.statements.get(programCounter);
      // if (statement != null) {
      //   console.log("evaluating", statement.label, statement.type);
      // }
      switch (statement?.type) {
        case "call":
          this.evaluateCall(statement);
          break;
        case "assignment":
          this.evaluateAssignment(statement);
      }
      programCounter += 1;
    }
  }

  evaluateAssignment(
    statement: RefinedStatement<"assignment">,
  ): void {
    const { lhs, rhs } = statement;
    const value = this.evaluateExpression(rhs, statement);
    const dest = parseReferenceIdentifier(lhs.identifier, statement);
    switch (dest.type) {
      case "local":
        return this.setLocal(dest.keyOrName, value, statement);
      case "point":
        return this.setPoint(dest.name, value);
    }
    throw runtimeError(`cannot assign to ${lhs.identifier}`, statement);
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

  getPoint(name: string, context: LineContext): number {
    const value = this.#points.get(name);
    if (value == null) {
      throw runtimeError(`point ${name} is not defined`, context);
    }
    return value;
  }

  setPoint(name: string, value: number): void {
    this.#points.set(name, value);
  }

  evaluateExpression(expression: Expression, context: LineContext): number {
    switch (expression.type) {
      case "literal": {
        const token = expression.token;
        switch (token.type) {
          case "number":
            return token.number;
        }
      }
    }
    throw runtimeError("invalid expression", context);
  }
}
