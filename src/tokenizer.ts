import { LineContext, parsingError } from "./errors.js";
import {
  INFIX_BINARY_OPERATORS,
  InfixBinaryOperatorType,
  KeywordSet,
  KeywordsType,
  SYMBOLS,
  SymbolsType,
} from "./reserved.js";

export type Token = Readonly<
  | {
    type: "number";
    number: number;
  }
  | {
    type: "name";
    name: string;
  }
  | {
    type: "ibop";
    operator: InfixBinaryOperatorType;
    precedence: number;
  }
  | { type: SymbolsType }
  | { type: KeywordsType }
>;

const tokenizers: ReadonlyArray<
  Readonly<{
    regex: RegExp;
    tokenize: (match: RegExpExecArray) => Token;
  }>
> = [
  {
    regex: new RegExp(`[\\${SYMBOLS.join("\\")}]`, "y"),
    tokenize: (match) => ({
      type: match[0] as SymbolsType,
    }),
  },
  {
    regex: new RegExp(
      Object.keys(INFIX_BINARY_OPERATORS)
        .map((op) => op.length === 1 ? `\\${op}` : op.replace(".", "\\."))
        .join("|"),
      "y",
    ),
    tokenize: (match) => {
      const op = match[0] as InfixBinaryOperatorType;
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
          type: name as KeywordsType,
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

export function tokenizeLine(
  line: string,
  context: LineContext,
): readonly Token[] {
  const tokens: Array<Token> = [];
  let index = 0;
  outer:
  while (index < line.length) {
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
