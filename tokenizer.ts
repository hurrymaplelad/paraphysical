import { LineContext, parsingError } from "./errors.ts";

export const SYMBOLS = ["(", ")", ",", "="] as const;
export type SymbolsType = typeof SYMBOLS[number];

export const KEYWORDS = ["IF", "THEN", "ELSE"] as const;
export type KeywordsType = typeof KEYWORDS[number];
export const KeywordSet: Set<string> = new Set(KEYWORDS);

// Values are precedence, lower wins
export const INFIX_BINARY_OPERATORS = {
  // 3
  ".ROOT.": 3,
  // 4
  "*": 4,
  "/": 4,
  // 5
  "+": 5,
  "-": 5,
  // 6
  ".EQ.": 6,
  ".NE.": 6,
  ".GT.": 6,
  ".GE.": 6,
  ".LT.": 6,
  ".LE.": 6,
  // 7
  ".AND.": 7,
  ".NAND.": 7,
  // 8
  ".OR.": 8,
  ".XOR.": 8,
} as const;
export type InfixBinaryOperatorType = keyof typeof INFIX_BINARY_OPERATORS;

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
    regex: /[A-Za-z][A-Za-z0-9]*/y,
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
