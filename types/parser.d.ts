import ArrayIterator from "./array_iterator.js";
import { LineContext } from "./errors.js";
import { InfixBinaryOperatorType, ResidentPointNameType, StatusNameType } from "./reserved.js";
import { Token } from "./tokenizer.js";
export declare type StatementContext = Readonly<LineContext & {
    label: number;
}>;
export declare type Statement = Readonly<StatementContext & ({
    type: "comment";
    comment: string;
} | {
    type: "call";
    functionName: string;
    args: readonly Expression[];
} | {
    type: "assignment";
    lhs: RefinedExpression<"reference">;
    rhs: Expression;
} | {
    type: "conditional";
    condition: Expression;
    then: Statement;
    else: Statement | null;
} | {
    type: "GOTO";
    destinationLabel: number;
} | {
    type: "GOSUB";
    destinationLabel: number;
    args: ReadonlyArray<RefinedExpression<"reference">>;
} | {
    type: "RETURN";
} | {
    type: "SAMPLE";
    secondsPerSample: number;
    sampledStatement: Statement;
})>;
export declare type RefinedStatement<Type extends Statement["type"]> = Extract<Statement, Record<"type", Type>>;
export declare type ParsedFile = Readonly<{
    statements: Map<number, Statement>;
    maxLabel: number;
}>;
export declare function parseFile(contents: string, context: Readonly<{
    filename: string;
}>): ParsedFile;
export declare function parseLine(line: string, context: LineContext): Statement;
declare type Tokens = ArrayIterator<Token>;
declare type RefinedToken<Type extends Token["type"]> = Extract<Token, Record<"type", Type>>;
export declare function parseStatement(tokens: Tokens, context: StatementContext): Statement;
export declare function parseSAMPLE(tokens: Tokens, context: StatementContext): Statement;
export declare function parseConditional(tokens: Tokens, context: StatementContext): Statement;
export declare function parseCallStatement(tokens: Tokens, context: StatementContext): Statement;
export declare function parseAssignment(tokens: Tokens, context: StatementContext): Statement;
export declare function parseGOTO(tokens: Tokens, context: StatementContext): Statement;
export declare const MAX_GOSUB_ARG_COUNT = 15;
export declare function parseGOSUB(tokens: Tokens, context: StatementContext): Statement;
export declare type Expression = Readonly<{
    type: "reference";
    identifier: string;
} | {
    type: "literal";
    token: RefinedToken<"number">;
} | {
    type: "ibop";
    operator: InfixBinaryOperatorType;
    lhs: Expression;
    rhs: Expression;
} | {
    type: "call";
    functionName: string;
    arg: Expression;
}>;
export declare type RefinedExpression<Type extends Expression["type"]> = Extract<Expression, Record<"type", Type>>;
export declare function parseExpression(tokens: Tokens, context: LineContext, initialLHS?: Expression | null): Expression;
export declare function parseCallExpression(tokens: Tokens, context: LineContext): Expression;
export declare function parseLiteral(tokens: Tokens, context: LineContext): Expression;
export declare function parseReference(tokens: Tokens, context: LineContext): RefinedExpression<"reference">;
export declare type ReferenceIdentifier = Readonly<{
    type: "local";
    keyOrName: string;
} | {
    type: "point";
    name: string;
} | {
    type: "residentPoint";
    name: ResidentPointNameType;
} | {
    type: "status";
    name: StatusNameType;
}>;
export declare function parseReferenceIdentifier(identifier: string, _context: LineContext): ReferenceIdentifier;
export {};
