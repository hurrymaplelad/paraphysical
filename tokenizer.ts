import { LineContext, parsingError } from "./errors.ts";

const SYMBOLS = ["(", ")", ",", "="] as const;
type SymbolsType = typeof SYMBOLS[number];

export type Token = Readonly<
  | {
    type: "number";
    number: number;
  }
  | {
    type: "name";
    name: string;
  }
  | { type: SymbolsType }
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
      type: (match[0] as SymbolsType),
    }),
  },
  {
    regex: /[1-9][0-9]*(\.[0-9]+)?/y,
    tokenize: (match) => ({
      type: "number",
      number: Number(match[0]),
    }),
  },
  {
    regex: /[A-Za-z][A-Za-z0-9]*/y,
    tokenize: (match) => ({
      type: "name",
      name: match[0],
    }),
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
): ReadonlyArray<Token> {
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
