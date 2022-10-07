import { LineContext, makeParseError } from "./errors.ts";

type Token = Readonly<
  {
    type: "number";
    number: number;
  } | {
    type: "keyword";
    word: string;
  }
>;

const tokenizers: ReadonlyArray<
  Readonly<{
    regex: RegExp;
    tokenize: (match: RegExpExecArray) => Token;
  }>
> = [
  {
    regex: /^[1-9][0-9]*(\.[0-9]+)?/g,
    tokenize: (match) => ({
      type: "number",
      number: Number(match[0]),
    }),
  },
];

export function tokenizeLine(
  line: string,
  context: LineContext,
): ReadonlyArray<Token> {
  const tokens: Array<Token> = [];
  let index = 0;
  outer:
  while (index < line.length) {
    for (const { regex, tokenize } of tokenizers) {
      regex.lastIndex = index;
      const match = regex.exec(line);
      if (match != null) {
        tokens.push(tokenize(match));
        index = regex.lastIndex;
        continue outer;
      }
    }
    throw makeParseError(`Invalid token: ${line[index]}`, context);
  }
  return tokens;
}
