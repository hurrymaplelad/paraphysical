import { Clock } from "./clocks.js";
import { LineContext } from "./errors.js";
import { Expression, RefinedExpression, RefinedStatement, Statement } from "./parser.js";
import { ResidentPointNameType } from "./reserved.js";
import { DateTime } from "./datetime.js";
export declare class Interpreter {
    #private;
    readonly clock: Clock;
    timezone: string;
    constructor(options?: Readonly<{
        clock?: Clock;
        timezone?: string;
    }> | null);
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
    runOnce(filename: string): Iterable<void>;
    runOnceSync(filename: string): void;
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
    getSecondsCounter(name: string, context: LineContext): number;
    setSecondsCounter(name: string, value: number, context: LineContext): void;
    getResidentPoint(name: ResidentPointNameType, content: LineContext): number;
    evaluateExpression(expression: Expression, context: LineContext): number;
    evaluateLiteralExpression(expression: RefinedExpression<"literal">, _context: LineContext): number;
    evaluateReferenceExpression(expression: RefinedExpression<"reference">, context: LineContext): number;
    evaluateInfixBinaryOperation(expression: RefinedExpression<"ibop">, context: LineContext): number;
    evaluateCallExpression(expression: RefinedExpression<"call">, context: LineContext): number;
}
