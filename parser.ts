import ArrayIterator from "./array_iterator.ts";
import { LineContext, parsingError, unexpectedTokenError } from "./errors.ts";
import { Token, tokenizeLine } from "./tokenizer.ts";

export type StatementContext = Readonly<
  LineContext & {
    // The authored line number at the start of each line
    label: number;
  }
>;

export type Statement = Readonly<
  & StatementContext
  & (
    | {
      type: "comment";
      comment: string;
    }
    | {
      type: "call";
      functionName: string;
      args: ReadonlyArray<Expression>;
    }
    | {
      type: "assignment";
    }
  )
>;

//
// Text Parsers
///

export type ParsedFile = Map<number, Statement>;
export function parseFile(
  contents: string,
  context: Readonly<{ filename: string }>,
): ParsedFile {
  const parsed = new Map();
  for (const [i, line] of contents.split("\n").entries()) {
    const statement = parseLine(line, { ...context, sourceLineNumber: i + 1 });
    parsed.set(statement.label, statement);
  }
  return parsed;
}

export function parseLine(
  line: string,
  context: LineContext,
): Statement {
  // Line label
  const labelMatch = /^([0-9]{5})\s+(.*)$/.exec(line);
  if (labelMatch == null) {
    throw parsingError("Line must start with 5 digit label", context);
  }
  const [_, labelString, rest] = labelMatch;
  const label = parseInt(labelString, 10);

  // Comment
  if (rest.startsWith("C")) {
    return { type: "comment", comment: rest, label, ...context };
  }

  const tokenList = tokenizeLine(rest, context);
  const tokens = new ArrayIterator(tokenList);
  return parseStatementTokens(tokens, { label, ...context });
}

//
// Token Parsers
//

type Tokens = ArrayIterator<Token>;
// Union refinement: https://engineering.widen.com/blog/Demystifying-TypeScripts-Extract-Type/
type RefinedToken<Type extends Token["type"]> = Extract<
  Token,
  Record<"type", Type>
>;

export function parseStatementTokens(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  const first = tokens.peek(0);
  const second = tokens.peek(1);
  switch (first?.type) {
    case "name":
      switch (second?.type) {
        case "(":
          return parseCall(tokens, context);
      }
  }
  throw unexpectedTokenError({ actual: first }, context);
}

export function parseCall(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  const functionName = consumeExpected(tokens, "name", context);
  consumeExpected(tokens, "(", context);
  const args = [];
  while (!tokens.isDone() && tokens.peek()?.type != ")") {
    args.push(parseArg(tokens, context));
    if (tokens.peek()?.type === ",") {
      tokens.skip(1);
    }
  }
  consumeExpected(tokens, ")", context);
  return { type: "call", functionName: functionName.name, args, ...context };
}

type Expression = Readonly<{
  type: "reference";
  identifier: string;
}>;

export function parseArg(
  tokens: Tokens,
  context: StatementContext,
): Expression {
  const first = tokens.next()?.value;
  switch (first?.type) {
    case "name":
      return {
        type: "reference",
        identifier: first.name,
      };
  }
  throw unexpectedTokenError({
    actual: first,
  }, context);
}

//
// Utilities
//

function consumeExpected<Type extends Token["type"]>(
  tokens: Tokens,
  expected: Type,
  context: LineContext,
  expectedLabel = "",
): RefinedToken<Type> {
  const { done: missing, value: token } = tokens.next();
  if (missing || token.type !== expected) {
    throw unexpectedTokenError({
      expected: expectedLabel || expected,
      actual: token,
    }, context);
  }
  return token as RefinedToken<Type>;
}
