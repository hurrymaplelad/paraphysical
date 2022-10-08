import ArrayIterator from "./array_iterator.ts";
import { LineContext, parsingError, unexpectedTokenError } from "./errors.ts";
import { Token, tokenizeLine } from "./tokenizer.ts";

const MAX_LINE_LABEL = 32767;

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
      args: readonly Expression[];
    }
    | {
      type: "assignment";
      lhs: RefinedExpression<"reference">;
      rhs: Expression;
    }
    | {
      type: "conditional";
      condition: Expression;
      then: Statement;
      else: Statement | null;
    }
  )
>;

// Union refinement: https://engineering.widen.com/blog/Demystifying-TypeScripts-Extract-Type/
export type RefinedStatement<Type extends Statement["type"]> = Extract<
  Statement,
  Record<"type", Type>
>;

//
// Text Parsers
///

export type ParsedFile = Readonly<{
  statements: Map<number, Statement>;
  maxLabel: number;
}>;

export function parseFile(
  contents: string,
  context: Readonly<{ filename: string }>,
): ParsedFile {
  const statements = new Map();
  let maxLabel = 0;
  for (const [i, line] of contents.split("\n").entries()) {
    const statement = parseLine(line, { ...context, sourceLineNumber: i + 1 });
    statements.set(statement.label, statement);
    maxLabel = Math.max(maxLabel, statement.label);
  }
  return { statements, maxLabel };
}

export function parseLine(
  line: string,
  context: LineContext,
): Statement {
  // Line label
  const labelMatch = /^([0-9]{1,5})\s+(.*)$/.exec(line);
  const labelError = parsingError(
    `Line must start with number between 1 and ${MAX_LINE_LABEL}`,
    context,
  );
  if (labelMatch == null) {
    throw labelError;
  }
  const [_, labelString, rest] = labelMatch;
  const label = parseInt(labelString, 10);
  if (label === 0 || label > MAX_LINE_LABEL) {
    throw labelError;
  }

  // Comment
  if (rest.startsWith("C")) {
    return { type: "comment", comment: rest, label, ...context };
  }

  const tokenList = tokenizeLine(rest, context);
  const tokens = new ArrayIterator(tokenList);
  const statement = parseStatement(tokens, { label, ...context });

  // Check for trailing tokens
  const trailing = tokens.next().value;
  if (trailing != null) {
    throw unexpectedTokenError({ actual: trailing }, context);
  }

  return statement;
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

export function parseStatement(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  const first = tokens.peek(0);
  const second = tokens.peek(1);
  switch (first?.type) {
    case "IF":
      return parseConditional(tokens, context);
    case "name":
      switch (second?.type) {
        case "(":
          return parseCall(tokens, context);
        case "=":
          return parseAssignment(tokens, context);
      }
  }
  throw unexpectedTokenError({ actual: first }, context);
}

//
// Statement Parsers
//

export function parseConditional(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  consumeExpected(tokens, "IF", context);
  consumeExpected(tokens, "(", context);
  const condition = parseExpression(tokens, context);
  consumeExpected(tokens, ")", context);
  consumeExpected(tokens, "THEN", context);
  const thn = parseStatement(tokens, context);
  let els = null;
  if (tokens.peek()?.type === "ELSE") {
    tokens.skip(1);
    els = parseStatement(tokens, context);
  }
  return {
    ...context,
    type: "conditional",
    condition,
    then: thn,
    else: els,
  };
}

export function parseCall(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  const functionName = consumeExpected(tokens, "name", context);
  consumeExpected(tokens, "(", context);
  const args = [];
  while (!tokens.isDone() && tokens.peek()?.type != ")") {
    args.push(parseExpression(tokens, context));
    if (tokens.peek()?.type === ",") {
      tokens.skip(1);
    }
  }
  consumeExpected(tokens, ")", context);
  return { type: "call", functionName: functionName.name, args, ...context };
}

export function parseAssignment(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  const lhs = parseReference(tokens, context);
  consumeExpected(tokens, "=", context);
  const rhs = parseExpression(tokens, context);
  return {
    ...context,
    type: "assignment",
    lhs,
    rhs,
  };
}

//
// Expression Parsers
//

export type Expression = Readonly<
  | {
    type: "reference";
    identifier: string;
  }
  | {
    type: "literal";
    token: RefinedToken<"number">;
  }
>;

// Union refinement: https://engineering.widen.com/blog/Demystifying-TypeScripts-Extract-Type/
export type RefinedExpression<Type extends Expression["type"]> = Extract<
  Expression,
  Record<"type", Type>
>;

export function parseExpression(
  tokens: Tokens,
  context: StatementContext,
): Expression {
  const first = tokens.peek();
  switch (first?.type) {
    case "name":
      return parseReference(tokens, context);
    case "number":
      return parseLiteral(tokens, context);
  }
  throw unexpectedTokenError({
    actual: first,
  }, context);
}

export function parseLiteral(
  tokens: Tokens,
  context: StatementContext,
): Expression {
  const next = tokens.next().value;
  switch (next?.type) {
    case "number":
      return {
        type: "literal",
        token: next,
      };
  }
  throw unexpectedTokenError({ actual: next }, context);
}

export function parseReference(
  tokens: Tokens,
  context: StatementContext,
): RefinedExpression<"reference"> {
  const nameToken = consumeExpected(tokens, "name", context);
  return {
    type: "reference",
    identifier: nameToken.name,
  };
}

export type ReferenceIdentifier = Readonly<
  | {
    type: "local";
    keyOrName: string;
  }
  | {
    type: "point";
    name: string;
  }
>;

export function parseReferenceIdentifier(
  identifier: string,
  _context: LineContext,
): ReferenceIdentifier {
  if (identifier.startsWith("$")) {
    return {
      type: "local",
      keyOrName: identifier.slice(1),
    };
  }
  return {
    type: "point",
    name: identifier,
  };
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
