import { Clock } from "./clocks.js";
import { LineContext } from "./errors.js";
import { Expression, RefinedExpression, RefinedStatement, Statement } from "./parser.js";
import { ResidentPointNameType } from "./reserved.js";
import { DateTime } from "./datetime.js";
export declare type FileEvaluationState = {
    disabledLabels: Set<number>;
    gosubStack: number[];
    readonly locals: Map<string, number>;
    programCounter: number;
    readonly statementStates: Map<number, StatementState>;
    readonly secondsCounterAssignmentTimestamps: Map<string, number>;
    timestampAtStartOfLatestRun: number;
};
export declare type StatementState = {
    readonly type: "SAMPLE";
    lastRunTimestamp: number;
};
export declare class Interpreter {
    #private;
    readonly clock: Clock;
    timezone: string;
    constructor(options?: Readonly<{
        clock?: Clock;
        timezone?: string;
    }> | null);
    /**
     * Parses the program text for a file.
     * Loading the same file again will reset state for that file.
     * Use `getFileState()` + `loadFileState()` to preserve state
     * across loads.
     */
    load(filename: string, contents: string): void;
    dateTime(): DateTime;
    /**
     * Run the argument file until the first time the final line
     * executes. Yields after each line.
     *
     * Program state will remain in this interpreter. For example,
     * if the final statement was a GOTO, the programCounter will point
     * to the GOTO line if the file is run again.
     */
    runOnce(filename: string): IterableIterator<void>;
    static findNextStatement(firstCandidate: number, maxLabel: number, statements: Map<number, Statement>): Statement;
    runOnceSync(filename: string): void;
    /**
     * The returned object is the live, mutable state used
     * by the interpreter. Callers may mutate things like local
     * values and the interpreter will reflect the changes.
     *
     * Use `strucuturedClone()` to make a copy.
     */
    getFileState(filename: string): FileEvaluationState;
    /**
     * Mutates the argument state.
     * Mismatched statement states are reset.
     */
    loadFileState(filename: string, state: FileEvaluationState): void;
    evaluateStatement(statement: Statement): void;
    evaluateConditional(statement: RefinedStatement<"conditional">): void;
    evaluateAssignment(statement: RefinedStatement<"assignment">): void;
    evaluateGOTO(statement: RefinedStatement<"GOTO">): void;
    evaluateGOSUB(statement: RefinedStatement<"GOSUB">): void;
    evaluateRETURN(statement: RefinedStatement<"RETURN">): void;
    evaluateSAMPLE(statement: RefinedStatement<"SAMPLE">): void;
    evaluateCallStatement(statement: RefinedStatement<"call">): void;
    evaluateLOCAL(args: readonly Expression[], context: LineContext): void;
    evaluateENABLE(args: readonly Expression[], context: LineContext): void;
    evaluateDISABLE(args: readonly Expression[], context: LineContext): void;
    getLocal(nameOrKey: string, context: LineContext): number;
    setLocal(nameOrKey: string, value: number, context: LineContext): void;
    getPoint(name: string, context: LineContext): number;
    setPoint(name: string, value: number): void;
    getPointsForDebug(): Map<string, number>;
    getStatementForDebug(filename: string, label: number): Statement | undefined;
    getSecondsCounter(name: string, context: LineContext): number;
    setSecondsCounter(name: string, value: number, context: LineContext): void;
    getResidentPoint(name: ResidentPointNameType, content: LineContext): number;
    evaluateExpression(expression: Expression, context: LineContext): number;
    evaluateLiteralExpression(expression: RefinedExpression<"literal">, _context: LineContext): number;
    evaluateReferenceExpression(expression: RefinedExpression<"reference">, context: LineContext): number;
    evaluateInfixBinaryOperation(expression: RefinedExpression<"ibop">, context: LineContext): number;
    evaluateCallExpression(expression: RefinedExpression<"call">, context: LineContext): number;
}
