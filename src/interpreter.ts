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
import { LineLabelNumber } from "./numbers.ts";
import {
  ResidentPointNameType,
  SECONDS_COUNTER_REGEX,
  STATUS_NAMES,
} from "./reserved.ts";
import { DateTime } from "./datetime.ts";

const LOCAL_DELIMITER = ":" as const;
const MAX_GOSUB_STACK_DEPTH = 8;

export type FileEvaluationState = {
  disabledLabels: Set<number>;
  gosubStack: number[];
  readonly locals: Map<string, number>;
  programCounter: number;
  readonly statementStates: Map<number, StatementState>;
  readonly secondsCounterAssignmentTimestamps: Map<string, number>;
  timestampAtStartOfLatestRun: number;
};

export type StatementState = {
  readonly type: "SAMPLE";
  lastRunTimestamp: number;
};

type RefinedStatementState<Type extends StatementState["type"]> = Extract<
  StatementState,
  Record<"type", Type>
>;

const DEFAULT_STATEMENT_STATES: {
  [P in StatementState["type"]]: () => Extract<StatementState, { type: P }>;
} = {
  SAMPLE: () => ({
    type: "SAMPLE",
    lastRunTimestamp: NaN,
  }),
};

const DEFAULT_RESIDENT_POINT_VALUES = {
  $BATT: STATUS_NAMES.OK,
} as const;

export class Interpreter {
  #files: Map<string, [ParsedFile, FileEvaluationState]>;
  // Key is filename:variablen. Use getLocal() / setLocal()
  #points: Map<string, number>;
  #currentFilename = "";
  readonly clock: Clock;
  timezone = "America/Los_Angeles"; // IANA zone name

  constructor(
    options: Readonly<{ clock?: Clock; timezone?: string }> | null = null,
  ) {
    this.#files = new Map();
    this.#points = new Map();
    this.clock = options?.clock ?? new SystemClock();
    if (options?.timezone != null) {
      this.timezone = options.timezone;
    }

    // Initialize resident points
    for (const [name, value] of Object.entries(DEFAULT_RESIDENT_POINT_VALUES)) {
      this.#points.set(name, value);
    }
  }

  /**
   * Parses the program text for a file.
   * Loading the same file again will reset state for that file.
   * Use `getFileState()` + `loadFileState()` to preserve state
   * across loads.
   */
  load(filename: string, contents: string): void {
    const parsedFile = parseFile(contents, { filename });
    const state = {
      disabledLabels: new Set<number>(),
      gosubStack: [],
      locals: new Map(),
      programCounter: 0,
      statementStates: new Map(),
      timestampAtStartOfLatestRun: NaN,
      secondsCounterAssignmentTimestamps: new Map(),
    };
    this.#files.set(filename, [parsedFile, state]);
  }

  dateTime(): DateTime {
    return new DateTime(this.clock.getTimestamp(), this.timezone);
  }

  /**
   * Run the argument file until the first time the final line
   * executes. Yields after each line.
   *
   * Program state will remain in this interpreter. For example,
   * if the final statement was a GOTO, the programCounter will point
   * to the GOTO line if the file is run again.
   */
  *runOnce(filename: string): IterableIterator<void> {
    const record = this.#files.get(filename);
    if (record == null) {
      throw fileNotLoaded(filename);
    }
    const [{ statements, maxLabel }, state] = record;
    this.#currentFilename = filename;
    state.timestampAtStartOfLatestRun = this.clock.getTimestamp();

    if (maxLabel <= 0) {
      return;
    }
    let previousStatement;
    while (true) {
      const nextCandidate = state.programCounter;
      const statement = Interpreter.findNextStatement(
        nextCandidate,
        maxLabel,
        statements,
      );
      state.programCounter = statement.label;
      // Check if we finished a pass of the file
      if (previousStatement?.label === maxLabel) {
        return;
      } else {
        yield;
      }

      // It's important to increment the program counter *before* evaluating
      // the statement in case it's a GOTO or similar.
      state.programCounter = Interpreter.findNextStatement(
        statement.label + 1,
        maxLabel,
        statements,
      ).label;
      //   console.log("evaluating", statement.label, statement.type);
      if (!state.disabledLabels.has(statement.label)) {
        this.evaluateStatement(statement);
      }
      previousStatement = statement;
    }
  }

  static findNextStatement(
    firstCandidate: number,
    maxLabel: number,
    statements: Map<number, Statement>,
  ): Statement {
    let statement;
    let label = firstCandidate;
    while (statement == null) {
      statement = statements.get(label);
      label = label >= maxLabel ? 1 : label + 1;
    }
    return statement;
  }

  runOnceSync(filename: string): void {
    for (const _ of this.runOnce(filename)) {
      // consume line
    }
  }

  #currentFileState(context: LineContext): FileEvaluationState {
    const record = this.#files.get(this.#currentFilename);
    if (record == null) {
      throw runtimeError("No running file", context);
    }
    return record[1];
  }

  /**
   * The returned object is the live, mutable state used
   * by the interpreter. Callers may mutate things like local
   * values and the interpreter will reflect the changes.
   *
   * Use `strucuturedClone()` to make a copy.
   */
  getFileState(filename: string): FileEvaluationState {
    const context = this.#files.get(filename);
    if (context == undefined) {
      throw new Error(`${filename} not loaded`);
    }
    return context[1];
  }

  /**
   * Mutates the argument state.
   * Mismatched statement states are reset.
   */
  loadFileState(filename: string, state: FileEvaluationState): void {
    const context = this.#files.get(filename);
    if (context == undefined) {
      throw new Error(`${filename} not loaded`);
    }
    const [parsed] = context;
    // Sanitize statement state
    for (const [label, { type }] of state.statementStates.entries()) {
      const parsedStatement = parsed.statements.get(label);
      if (parsedStatement == null || parsedStatement.type !== type) {
        state.statementStates.delete(label);
      }
    }
    this.#files.set(filename, [parsed, state]);
  }

  #statementState<
    S extends Extract<Statement, { type: StatementState["type"] }>,
  >(statement: S): RefinedStatementState<S["type"]> {
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
        return this.evaluateCallStatement(statement);
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

  evaluateAssignment(statement: RefinedStatement<"assignment">): void {
    const { lhs, rhs } = statement;
    const value = this.evaluateExpression(rhs, statement);
    const dest = parseReferenceIdentifier(lhs.identifier, statement);
    switch (dest.type) {
      case "local":
        return this.setLocal(dest.keyOrName, value, statement);
      case "point":
        if (SECONDS_COUNTER_REGEX.test(dest.name)) {
          return this.setSecondsCounter(dest.name, value, statement);
        }
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
      throw runtimeError("RETURN outside GOSUB", statement);
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

  evaluateCallStatement(statement: RefinedStatement<"call">): void {
    const { functionName, args } = statement;

    switch (functionName.toUpperCase()) {
      case "LOCAL":
        return this.evaluateLOCAL(args, statement);

      case "ENABLE":
      case "ACT":
        return this.evaluateENABLE(args, statement);
      case "DISABLE":
      case "DEACT":
        return this.evaluateDISABLE(args, statement);
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

  evaluateENABLE(args: readonly Expression[], context: LineContext): void {
    for (const arg of args) {
      if (
        arg.type !== "literal" ||
        !LineLabelNumber.isValid(arg.token.number)
      ) {
        throw runtimeError(
          `ENABLE/ACT arg must be ${LineLabelNumber.description}`,
          context,
        );
      }
      const lineLabel = arg.token.number;
      this.#currentFileState(context).disabledLabels.delete(lineLabel);
    }
  }

  evaluateDISABLE(args: readonly Expression[], context: LineContext): void {
    for (const arg of args) {
      if (
        arg.type !== "literal" ||
        !LineLabelNumber.isValid(arg.token.number)
      ) {
        throw runtimeError(
          `DISABLE/DEACT arg must be ${LineLabelNumber.description}`,
          context,
        );
      }
      const lineLabel = arg.token.number;
      this.#currentFileState(context).disabledLabels.add(lineLabel);
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

  getPointsForDebug(): Map<string, number> {
    return this.#points;
  }

  getStatementForDebug(filename: string, label: number): Statement | undefined {
    return this.#files.get(filename)?.[0]?.statements?.get(label);
  }

  getSecondsCounter(name: string, context: LineContext): number {
    const assignmentTime =
      this.#currentFileState(context).secondsCounterAssignmentTimestamps.get(
        name,
      ) ?? this.clock.initialTimestamp();
    const assignedCount = this.#points.get(name) ?? 0;
    return Math.floor(
      this.clock.getTimestamp() - assignmentTime + assignedCount,
    );
  }

  setSecondsCounter(name: string, value: number, context: LineContext): void {
    const assignmentTime = this.clock.getTimestamp();
    this.#currentFileState(context).secondsCounterAssignmentTimestamps.set(
      name,
      assignmentTime,
    );
    this.#points.set(name, value);
  }

  getResidentPoint(name: ResidentPointNameType, content: LineContext): number {
    switch (name) {
      case "DAY":
        return this.dateTime().weekdayNumber();
      case "DAYOFM":
        return this.dateTime().dayOfMonth();
      case "CRTIME":
      case "TIME": // TODO - enforce that TIME type is not assignable
        return this.dateTime().fractionalHour();
      case "MONTH":
        return this.dateTime().monthNumber();
    }
    return this.getPoint(name, content);
  }

  evaluateExpression(expression: Expression, context: LineContext): number {
    switch (expression.type) {
      case "literal":
        return this.evaluateLiteralExpression(expression, context);
      case "reference":
        return this.evaluateReferenceExpression(expression, context);
      case "ibop":
        return this.evaluateInfixBinaryOperation(expression, context);
      case "call":
        return this.evaluateCallExpression(expression, context);
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
    const id = parseReferenceIdentifier(expression.identifier, context);
    switch (id.type) {
      case "local":
        return this.getLocal(id.keyOrName, context);
      case "point":
        if (SECONDS_COUNTER_REGEX.test(id.name)) {
          return this.getSecondsCounter(id.name, context);
        }
        return this.getPoint(id.name, context);
      case "residentPoint":
        return this.getResidentPoint(id.name, context);
      case "status":
        return STATUS_NAMES[id.name];
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

  evaluateCallExpression(
    expression: RefinedExpression<"call">,
    context: LineContext,
  ): number {
    const { functionName, arg } = expression;
    const argValue = this.evaluateExpression(arg, context);
    switch (functionName) {
      case "ATN":
        return Math.atan(argValue);
      case "COM":
        return 1 - (argValue ? 1 : 0);
      case "COS":
        return Math.cos(argValue);
      case "EXP":
        return Math.exp(argValue);
      case "LOG":
        return Math.log(argValue);
      case "SIN":
        return Math.sin(argValue);
      case "SQRT":
        return Math.sqrt(argValue);
      case "TAN":
        return Math.tan(argValue);
    }
    throw runtimeError(`unrecognized function: ${functionName}`, context);
  }
}
