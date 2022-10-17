import { Statement } from "./parser.js";
import { Token } from "./tokenizer.js";
export declare type LineContext = Readonly<{
    filename: string;
    sourceLineNumber: number;
}>;
export declare function printToken(token: Token | null): string;
export declare function errorAddress({ filename, sourceLineNumber }: LineContext): string;
export declare function runtimeError(message: string, context: LineContext): Error;
export declare function parsingError(message: string, context: LineContext): Error;
export declare function unexpectedTokenError({ expected, actual }: Readonly<{
    expected?: string;
    actual: Token | null;
}>, context: LineContext): Error;
export declare function invalidStatementError({ outer, inner }: Readonly<{
    outer: Statement["type"];
    inner: Statement["type"];
}>, context: LineContext): Error;
export declare function fileNotLoaded(filename: string): Error;
export declare function assertNever(_: never): void;
