import { Clock, SystemClock } from "./clocks.ts";
import {
  assertNever,
  fileNotLoaded,
  LineContext,
  runtimeError,
} from "./errors.ts";
import {
  Expression,
  ParsedFile,
  parseFile,
  parseReferenceIdentifier,
  RefinedExpression,
  RefinedStatement,
  Statement,
} from "./parser.ts";

const LOCAL_DELIMITER = ":" as const;
const MAX_GOSUB_STACK_DEPTH = 8;

type FileEvaluationState = {
  readonly locals: Map<string, number>;
  readonly statementStates: Map<number, StatementState>;
  programCounter: number;
  gosubStack: number[];
  timestampAtStartOfLatestRun: number;
};

type StatementState = {
  readonly type: "SAMPLE";
  lastRunTimestamp: number;
};

type RefinedStatementState<Type extends StatementState["type"]> = Extract<
  StatementState,
  Record<"type", Type>
>;

const DEFAULT_STATEMENT_STATES: {
  [P in StatementState["type"]]: () => Extract<
    StatementState,
    { type: P }
  >;
} = {
  SAMPLE: () => ({
    type: "SAMPLE",
    lastRunTimestamp: NaN,
  }),
};

export class Interpreter {
  #files: Map<string, [ParsedFile, FileEvaluationState]>;
  // Key is filename:variablen. Use getLocal() / setLocal()
  #points: Map<string, number>;
  #currentFilename = "";
  readonly clock: Clock;

  constructor(options: Readonly<{ clock?: Clock }> | null = null) {
    this.#files = new Map();
    this.#points = new Map();
    this.clock = options?.clock ?? new SystemClock();
  }

  load(filename: string, contents: string): void {
    const parsedFile = parseFile(contents, { filename });
    const state = {
      locals: new Map(),
      statementStates: new Map(),
      programCounter: 0,
      gosubStack: [],
      timestampAtStartOfLatestRun: NaN,
    };
    this.#files.set(filename, [parsedFile, state]);
  }

  /**
   * Run the argument file until the first time the final line
   * executes. Yields after each line.
   *
   * Program state will remain in this interpreter. For example,
   * if the final statement was a GOTO, the programCounter will point
   * to the GOTO line if the file is run again.
   */
  runOnceSync(
    filename: string,
  ): FileEvaluationState {
    const record = this.#files.get(filename);
    if (record == null) {
      throw fileNotLoaded(filename);
    }
    const [{ statements, maxLabel }, state] = record;
    this.#currentFilename = filename;
    state.timestampAtStartOfLatestRun = this.clock.getTimestamp();

    while (state.programCounter <= maxLabel) {
      const statement = statements.get(state.programCounter);
      // It's important to increment the program counter *before* evaluating
      // the statement in case it's a GOTO or similar.
      state.programCounter += 1;
      // if (statement != null) {
      //   console.log("evaluating", statement.label, statement.type);
      // }
      if (statement != null) {
        this.evaluateStatement(statement);
      }
      // Check if we reached the end of the file, which triggers
      // time increments and such.
      if (statement?.label === maxLabel) {
        break;
      }
    }
    return state;
  }

  #currentFileState(context: LineContext): FileEvaluationState {
    const record = this.#files.get(this.#currentFilename);
    if (record == null) {
      throw runtimeError("No running file", context);
    }
    return record[1];
  }

  #statementState<
    S extends Extract<Statement, { type: StatementState["type"] }>,
  >(
    statement: S,
  ): RefinedStatementState<S["type"]> {
    const { statementStates } = this.#currentFileState(statement);
    const { label } = statement;

    if (statementStates.has(label)) {
      return statementStates.get(label) as RefinedStatementState<S["type"]>;
    } else {
      const state = DEFAULT_STATEMENT_STATES[statement.type]();
      statementStates.set(label, state);
      return state as RefinedStatementState<S["type"]>;
    }
  }

  evaluateStatement(statement: Statement) {
    switch (statement.type) {
      case "assignment":
        return this.evaluateAssignment(statement);
      case "call":
        return this.evaluateCall(statement);
      case "comment":
        return;
      case "conditional":
        return this.evaluateConditional(statement);
      case "GOTO":
        return this.evaluateGOTO(statement);
      case "GOSUB":
        return this.evaluateGOSUB(statement);
      case "RETURN":
        return this.evaluateRETURN(statement);
      case "SAMPLE":
        return this.evaluateSAMPLE(statement);
      default:
        // Enforce exhaustiveness
        assertNever(statement);
    }
  }

  evaluateConditional(statement: RefinedStatement<"conditional">): void {
    const conditionValue = this.evaluateExpression(
      statement.condition,
      statement,
    );
    if (conditionValue) {
      this.evaluateStatement(statement.then);
    } else if (statement.else != null) {
      this.evaluateStatement(statement.else);
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

  evaluateGOTO(statement: RefinedStatement<"GOTO">): void {
    const { destinationLabel } = statement;
    this.#currentFileState(statement).programCounter = destinationLabel;
  }

  evaluateGOSUB(statement: RefinedStatement<"GOSUB">): void {
    const fileState = this.#currentFileState(statement);
    const { args, destinationLabel } = statement;
    if (fileState.gosubStack.length >= MAX_GOSUB_STACK_DEPTH) {
      throw runtimeError(
        `Maximum of ${MAX_GOSUB_STACK_DEPTH} nested GOSUB calls exceeded`,
        statement,
      );
    }
    fileState.gosubStack.push(statement.label);
    for (const [index, arg] of args.entries()) {
      const argValue = this.evaluateExpression(arg, statement);
      // Avoid setLocal() to bypass declaration check
      this.#currentFileState(statement).locals.set(
        "ARG" + (index + 1),
        argValue,
      );
    }
    fileState.programCounter = destinationLabel;
  }

  evaluateRETURN(statement: RefinedStatement<"RETURN">): void {
    const fileState = this.#currentFileState(statement);
    const callsiteLabel = fileState.gosubStack.pop();
    if (callsiteLabel == undefined) {
      throw runtimeError(
        "RETURN outside GOSUB",
        statement,
      );
    }
    fileState.programCounter = callsiteLabel + 1;
  }

  evaluateSAMPLE(statement: RefinedStatement<"SAMPLE">): void {
    const { sampledStatement, secondsPerSample } = statement;
    const fileState = this.#currentFileState(statement);
    const statementState = this.#statementState(statement);
    const { lastRunTimestamp } = statementState;
    const currentTime = fileState.timestampAtStartOfLatestRun;

    if (
      isNaN(lastRunTimestamp) ||
      currentTime - lastRunTimestamp >= secondsPerSample
    ) {
      statementState.lastRunTimestamp = currentTime;
      this.evaluateStatement(sampledStatement);
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
      if (name.indexOf(LOCAL_DELIMITER) > -1) {
        throw runtimeError(`invalid local declaration: ${name}`, context);
      }
      const key = this.#keyLocal(name, context);
      if (this.#isLocalDeclared(key)) {
        throw runtimeError(`attemped to re-declare LOCAL: ${name}`, context);
      }
      this.#currentFileState(context).locals.set(name, 0);
    }
  }

  #keyLocal(nameOrKey: string, context: LineContext): [string, string] {
    const parts = nameOrKey.split(LOCAL_DELIMITER);
    switch (parts.length) {
      case 1:
        return [this.#currentFilename, nameOrKey];
      case 2:
        return [parts[0], parts[1]];
      default:
        throw runtimeError(`invalid local: ${nameOrKey}`, context);
    }
  }

  #isLocalDeclared([filename, name]: [string, string]): boolean {
    return this.#files.get(filename)?.[1].locals.has(name) ?? false;
  }

  getLocal(nameOrKey: string, context: LineContext): number {
    const [filename, name] = this.#keyLocal(nameOrKey, context);

    const value = this.#files.get(filename)?.[1].locals.get(name);
    if (value === undefined) {
      throw runtimeError(
        `attempted to read undeclared LOCAL: ${nameOrKey}`,
        context,
      );
    }
    return value;
  }

  setLocal(nameOrKey: string, value: number, context: LineContext): void {
    const key = this.#keyLocal(nameOrKey, context);
    const [filename, name] = key;
    const fileState = this.#files.get(filename)?.[1];
    if (fileState == null || !this.#isLocalDeclared(key)) {
      throw runtimeError(`attempted to set undeclared LOCAL: ${key}`, context);
    }
    fileState.locals.set(name, value);
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
      case "literal":
        return this.evaluateLiteralExpression(expression, context);
      case "reference":
        return this.evaluateReferenceExpression(expression, context);
      case "ibop":
        return this.evaluateInfixBinaryOperation(expression, context);
    }
  }

  evaluateLiteralExpression(
    expression: RefinedExpression<"literal">,
    _context: LineContext,
  ): number {
    const token = expression.token;
    switch (token.type) {
      case "number":
        return token.number;
    }
  }

  evaluateReferenceExpression(
    expression: RefinedExpression<"reference">,
    context: LineContext,
  ): number {
    const id = parseReferenceIdentifier(
      expression.identifier,
      context,
    );
    switch (id.type) {
      case "local":
        return this.getLocal(id.keyOrName, context);
      case "point":
        return this.getPoint(id.name, context);
    }
  }

  evaluateInfixBinaryOperation(
    expression: RefinedExpression<"ibop">,
    context: LineContext,
  ): number {
    const lhsValue = this.evaluateExpression(expression.lhs, context);
    const rhsValue = () => this.evaluateExpression(expression.rhs, context);
    switch (expression.operator) {
      // Arithmetic
      case "+":
        return lhsValue + rhsValue();
      case "-":
        return lhsValue - rhsValue();
      case "*":
        return lhsValue * rhsValue();
      case "/":
        return lhsValue / rhsValue();
      case ".ROOT.":
        return Math.pow(lhsValue, 1 / rhsValue());

        // Comparison
      case ".EQ.":
        return Number(lhsValue === rhsValue());
      case ".NE.":
        return Number(lhsValue !== rhsValue());
      case ".GT.":
        return Number(lhsValue > rhsValue());
      case ".GE.":
        return Number(lhsValue >= rhsValue());
      case ".LT.":
        return Number(lhsValue < rhsValue());
      case ".LE.":
        return Number(lhsValue <= rhsValue());

      // Logical
      case ".AND.":
        return Number(lhsValue && rhsValue());
      case ".NAND.":
        return Number(!(lhsValue && rhsValue()));
      case ".OR.":
        return Number(lhsValue || rhsValue());
      case ".XOR.": {
        const rhsVal = rhsValue();
        return Number((lhsValue || rhsVal) && !(lhsValue && rhsVal));
      }
      default:
        assertNever(expression.operator);
        throw runtimeError(
          `operator ${expression.operator} not implemented`,
          context,
        );
    }
  }
}
