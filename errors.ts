import { Token } from "./tokenizer.ts";

export type LineContext = Readonly<{
  filename: string;
  // The implicit line number in source code, not the authored line number prefix
  sourceLineNumber: number;
}>;

export function printToken(token: Token | null): string {
  switch (token?.type) {
    case "name":
      return `"${token.name}"`;
    case "number":
      return token.number.toString();
    case "(":
    case ")":
    case "=":
    case ",":
      return token.type;
  }
  return JSON.stringify(token);
}

export function parsingError(
  message: string,
  { filename, sourceLineNumber }: LineContext,
): Error {
  return new Error(
    `Parse Error: ${message} [${filename}:${sourceLineNumber}]`,
  );
}

export function unexpectedTokenError({ expected, actual }: Readonly<{
  expected?: string;
  actual: Token | null;
}>, context: LineContext): Error {
  const expectation = expected ? `Expected ${expected}. Got ` : "";
  return parsingError(
    `Unexepected token: ${expectation}${printToken(actual)}`,
    context,
  );
}
