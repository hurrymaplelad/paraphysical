import ArrayIterator from "./array_iterator.ts";
import {
  invalidStatementError,
  LineContext,
  parsingError,
  unexpectedTokenError,
} from "./errors.ts";
import { LineLabelNumber, PositiveInt16 } from "./numbers.ts";
import { InfixBinaryOperatorType, ResidentPointNameType, ResidentPointSet, StatusNameType, STATUS_NAMES } from "./reserved.ts";
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
    | {
      type: "GOTO";
      destinationLabel: number;
    }
    | {
      type: "GOSUB";
      destinationLabel: number;
      args: ReadonlyArray<RefinedExpression<"reference">>;
    }
    | {
      type: "RETURN";
    }
    | {
      type: "SAMPLE";
      secondsPerSample: number;
      sampledStatement: Statement;
    }
  )
>;

const TIMED_STATEMENTS: Set<Statement["type"]> = new Set([
  "SAMPLE",
]);

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
    `Line must start with ${LineLabelNumber.description}`,
    context,
  );
  if (labelMatch == null) {
    throw labelError;
  }
  const [_, labelString, rest] = labelMatch;
  const label = parseInt(labelString, 10);
  if (!LineLabelNumber.isValid(label)) {
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
    case "GOTO":
      return parseGOTO(tokens, context);
    case "GOSUB":
      return parseGOSUB(tokens, context);
    case "RETURN":
      tokens.skip(1);
      return { ...context, type: "RETURN" };
    case "name":
      switch (second?.type) {
        case "(": {
          switch (first.name) {
            case "SAMPLE":
              return parseSAMPLE(tokens, context);
          }
          return parseCallStatement(tokens, context);
        }
        case "=":
          return parseAssignment(tokens, context);
      }
  }
  throw unexpectedTokenError({ actual: first }, context);
}

//
// Statement Parsers
//

export function parseSAMPLE(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  consumeExpected(tokens, "name", context);
  consumeExpected(tokens, "(", context);
  const { number: secondsPerSample } = consumeExpected(
    tokens,
    "number",
    context,
  );
  if (!PositiveInt16.isValid(secondsPerSample)) {
    throw parsingError(
      `SAMPLE seconds must be ${PositiveInt16.description}. Got ${secondsPerSample}`,
      context,
    );
  }
  consumeExpected(tokens, ")", context);
  const sampledStatement = parseStatement(tokens, context);
  if (TIMED_STATEMENTS.has(sampledStatement.type)) {
    throw invalidStatementError(
      { outer: "SAMPLE", inner: sampledStatement.type },
      context,
    );
  }
  return {
    ...context,
    type: "SAMPLE",
    secondsPerSample,
    sampledStatement,
  };
}

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
  validateConditionalSubStatement(thn.type, context);
  let els = null;
  if (tokens.peek()?.type === "ELSE") {
    tokens.skip(1);
    els = parseStatement(tokens, context);
    validateConditionalSubStatement(els.type, context);
  }
  return {
    ...context,
    type: "conditional",
    condition,
    then: thn,
    else: els,
  };
}

const FORBIDDEN_CONDITIONAL_SUB_STATEMENTS: Set<Statement["type"]> = new Set([
  "conditional",
  ...TIMED_STATEMENTS.values(),
]);

function validateConditionalSubStatement(
  type: Statement["type"],
  context: LineContext,
): void {
  if (FORBIDDEN_CONDITIONAL_SUB_STATEMENTS.has(type)) {
    throw invalidStatementError(
      { outer: "conditional", inner: type },
      context,
    );
  }
}

export function parseCallStatement(
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

export function parseGOTO(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  consumeExpected(tokens, "GOTO", context);
  const { number: label } = consumeExpected(tokens, "number", context);
  if (!LineLabelNumber.isValid(label)) {
    throw parsingError(
      `Invalid GOTO: ${label}. Line number must be ${LineLabelNumber.description}`,
      context,
    );
  }
  return {
    ...context,
    type: "GOTO",
    destinationLabel: label,
  };
}

export const MAX_GOSUB_ARG_COUNT = 15;

export function parseGOSUB(
  tokens: Tokens,
  context: StatementContext,
): Statement {
  consumeExpected(tokens, "GOSUB", context);
  const { number: label } = consumeExpected(tokens, "number", context);
  if (!LineLabelNumber.isValid(label)) {
    throw parsingError(
      `Invalid GOSUB: ${label}. Line number must be ${LineLabelNumber.description}`,
      context,
    );
  }
  const args = [];
  while (!tokens.isDone() && tokens.peek()?.type === "name") {
    if (args.length >= MAX_GOSUB_ARG_COUNT) {
      throw parsingError(
        `GOSUB supports at most ${MAX_GOSUB_ARG_COUNT} arguments`,
        context,
      );
    }
    args.push(parseReference(tokens, context));
    if (tokens.peek()?.type === ",") {
      tokens.skip(1);
    }
  }
  return {
    ...context,
    type: "GOSUB",
    destinationLabel: label,
    args,
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
  | {
    type: "ibop"; // infix binary operation
    operator: InfixBinaryOperatorType;
    lhs: Expression;
    rhs: Expression;
  }
  | {
    type: "call";
    functionName: string;
    arg: Expression;
  }
>;

// Union refinement: https://engineering.widen.com/blog/Demystifying-TypeScripts-Extract-Type/
export type RefinedExpression<Type extends Expression["type"]> = Extract<
  Expression,
  Record<"type", Type>
>;

export function parseExpression(
  tokens: Tokens,
  context: LineContext,
  initialLHS: Expression | null = null,
): Expression {
  const lhs = initialLHS ?? parseExpressionLHS(tokens, context);
  if (tokens.peek()?.type != "ibop") {
    return lhs;
  }
  const opToken = consumeExpected(tokens, "ibop", context);
  const rhs = parseExpressionRHS(tokens, opToken, context);

  return parseExpression(tokens, context, {
    type: "ibop",
    operator: opToken.operator,
    lhs,
    rhs,
  });
}

function parseExpressionLHS(tokens: Tokens, context: LineContext): Expression {
  const first = tokens.peek();
  switch (first?.type) {
    case "name":
      if (tokens.peek(1)?.type === "(") {
        return parseCallExpression(tokens, context);
      } else {
        return parseReference(tokens, context);
      }
    case "number":
      return parseLiteral(tokens, context);
    case "(": {
      tokens.skip();
      const exp = parseExpression(tokens, context);
      consumeExpected(tokens, ")", context);
      return exp;
    }
  }
  throw unexpectedTokenError({
    actual: first,
  }, context);
}

function parseExpressionRHS(
  tokens: Tokens,
  opToken: RefinedToken<"ibop">,
  context: LineContext,
): Expression {
  const next = tokens.peek();
  const nextNext = tokens.peek(1);
  if (
    // Prioritize parenthesized expressions
    next?.type === "(" ||
    // Defer to higher precedence opertators
    (nextNext?.type === "ibop" && nextNext.precedence < opToken.precedence)
  ) {
    return parseExpression(tokens, context);
  } else {
    return parseExpressionLHS(tokens, context);
  }
}

export function parseCallExpression(
  tokens: Tokens,
  context: LineContext,
): Expression {
  const functionNameToken = consumeExpected(tokens, "name", context);
  consumeExpected(tokens, "(", context);
  const arg = parseExpression(tokens, context);
  consumeExpected(tokens, ")", context);
  return {
    type: "call",
    functionName: functionNameToken.name,
    arg,
  }
}

export function parseLiteral(
  tokens: Tokens,
  context: LineContext,
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
  context: LineContext,
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
  | {
    type: "residentPoint";
    name: ResidentPointNameType;
  }
  | {
    type: "status";
    name: StatusNameType;
  }
>;

export function parseReferenceIdentifier(
  identifier: string,
  _context: LineContext,
): ReferenceIdentifier {
  if (identifier in STATUS_NAMES) {
    return {
      type: "status",
      name: identifier as StatusNameType,
    }
  }
  if((ResidentPointSet as Set<string>).has(identifier)){
    return {
      type: "residentPoint",
      name: identifier as ResidentPointNameType,
    };
  }
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
