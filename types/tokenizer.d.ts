import { LineContext } from "./errors.js";
import { InfixBinaryOperatorType, KeywordsType, SymbolsType } from "./reserved.js";
export declare type Token = Readonly<{
    type: "number";
    number: number;
} | {
    type: "name";
    name: string;
} | {
    type: "ibop";
    operator: InfixBinaryOperatorType;
    precedence: number;
} | {
    type: SymbolsType;
} | {
    type: KeywordsType;
}>;
export declare function tokenizeLine(line: string, context: LineContext): readonly Token[];
