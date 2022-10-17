import { parsingError } from "./errors.js";
import { INFIX_BINARY_OPERATORS, KeywordSet, SYMBOLS, } from "./reserved.js";
const tokenizers = [
    {
        regex: new RegExp(`[\\${SYMBOLS.join("\\")}]`, "y"),
        tokenize: (match) => ({
            type: match[0],
        }),
    },
    {
        regex: new RegExp(Object.keys(INFIX_BINARY_OPERATORS)
            .map((op) => op.length === 1 ? `\\${op}` : op.replace(".", "\\."))
            .join("|"), "y"),
        tokenize: (match) => {
            const op = match[0];
            return {
                type: "ibop",
                operator: op,
                precedence: INFIX_BINARY_OPERATORS[op],
            };
        },
    },
    {
        regex: /[0-9]+(\.[0-9]+)?/y,
        tokenize: (match) => ({
            type: "number",
            number: Number(match[0]),
        }),
    },
    {
        regex: /\$?[A-Za-z][A-Za-z0-9]*/y,
        tokenize: (match) => {
            const name = match[0];
            return KeywordSet.has(name)
                ? {
                    type: name,
                }
                : {
                    type: "name",
                    name,
                };
        },
    },
    {
        regex: /"([A-Za-z0-9:$ .,_'-]+)"/y,
        tokenize: (match) => ({
            type: "name",
            name: match[1],
        }),
    },
];
const whitespaceRegex = /\s+/y;
export function tokenizeLine(line, context) {
    const tokens = [];
    let index = 0;
    outer: while (index < line.length) {
        whitespaceRegex.lastIndex = index;
        if (whitespaceRegex.exec(line) != null) {
            index = whitespaceRegex.lastIndex;
            continue;
        }
        for (const { regex, tokenize } of tokenizers) {
            regex.lastIndex = index;
            const match = regex.exec(line);
            if (match != null) {
                tokens.push(tokenize(match));
                index = regex.lastIndex;
                continue outer;
            }
        }
        throw parsingError(`Invalid token: '${line[index]}'`, context);
    }
    return tokens;
}
//# sourceMappingURL=tokenizer.js.map