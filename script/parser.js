"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseReferenceIdentifier = exports.parseReference = exports.parseLiteral = exports.parseCallExpression = exports.parseExpression = exports.parseGOSUB = exports.MAX_GOSUB_ARG_COUNT = exports.parseGOTO = exports.parseAssignment = exports.parseCallStatement = exports.parseConditional = exports.parseSAMPLE = exports.parseStatement = exports.parseLine = exports.parseFile = void 0;
const array_iterator_js_1 = __importDefault(require("./array_iterator.js"));
const errors_js_1 = require("./errors.js");
const numbers_js_1 = require("./numbers.js");
const reserved_js_1 = require("./reserved.js");
const tokenizer_js_1 = require("./tokenizer.js");
const TIMED_STATEMENTS = new Set([
    "SAMPLE",
]);
function parseFile(contents, context) {
    const statements = new Map();
    let maxLabel = 0;
    for (const [i, line] of contents.split("\n").entries()) {
        const statement = parseLine(line, { ...context, sourceLineNumber: i + 1 });
        statements.set(statement.label, statement);
        maxLabel = Math.max(maxLabel, statement.label);
    }
    return { statements, maxLabel };
}
exports.parseFile = parseFile;
function parseLine(line, context) {
    // Line label
    const labelMatch = /^([0-9]{1,5})\s+(.*)$/.exec(line);
    const labelError = (0, errors_js_1.parsingError)(`Line must start with ${numbers_js_1.LineLabelNumber.description}`, context);
    if (labelMatch == null) {
        throw labelError;
    }
    const [_, labelString, rest] = labelMatch;
    const label = parseInt(labelString, 10);
    if (!numbers_js_1.LineLabelNumber.isValid(label)) {
        throw labelError;
    }
    // Comment
    if (rest.startsWith("C")) {
        return { type: "comment", comment: rest, label, ...context };
    }
    const tokenList = (0, tokenizer_js_1.tokenizeLine)(rest, context);
    const tokens = new array_iterator_js_1.default(tokenList);
    const statement = parseStatement(tokens, { label, ...context });
    // Check for trailing tokens
    const trailing = tokens.next().value;
    if (trailing != null) {
        throw (0, errors_js_1.unexpectedTokenError)({ actual: trailing }, context);
    }
    return statement;
}
exports.parseLine = parseLine;
function parseStatement(tokens, context) {
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
    throw (0, errors_js_1.unexpectedTokenError)({ actual: first }, context);
}
exports.parseStatement = parseStatement;
//
// Statement Parsers
//
function parseSAMPLE(tokens, context) {
    consumeExpected(tokens, "name", context);
    consumeExpected(tokens, "(", context);
    const { number: secondsPerSample } = consumeExpected(tokens, "number", context);
    if (!numbers_js_1.PositiveInt16.isValid(secondsPerSample)) {
        throw (0, errors_js_1.parsingError)(`SAMPLE seconds must be ${numbers_js_1.PositiveInt16.description}. Got ${secondsPerSample}`, context);
    }
    consumeExpected(tokens, ")", context);
    const sampledStatement = parseStatement(tokens, context);
    if (TIMED_STATEMENTS.has(sampledStatement.type)) {
        throw (0, errors_js_1.invalidStatementError)({ outer: "SAMPLE", inner: sampledStatement.type }, context);
    }
    return {
        ...context,
        type: "SAMPLE",
        secondsPerSample,
        sampledStatement,
    };
}
exports.parseSAMPLE = parseSAMPLE;
function parseConditional(tokens, context) {
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
exports.parseConditional = parseConditional;
const FORBIDDEN_CONDITIONAL_SUB_STATEMENTS = new Set([
    "conditional",
    ...TIMED_STATEMENTS.values(),
]);
function validateConditionalSubStatement(type, context) {
    if (FORBIDDEN_CONDITIONAL_SUB_STATEMENTS.has(type)) {
        throw (0, errors_js_1.invalidStatementError)({ outer: "conditional", inner: type }, context);
    }
}
function parseCallStatement(tokens, context) {
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
exports.parseCallStatement = parseCallStatement;
function parseAssignment(tokens, context) {
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
exports.parseAssignment = parseAssignment;
function parseGOTO(tokens, context) {
    consumeExpected(tokens, "GOTO", context);
    const { number: label } = consumeExpected(tokens, "number", context);
    if (!numbers_js_1.LineLabelNumber.isValid(label)) {
        throw (0, errors_js_1.parsingError)(`Invalid GOTO: ${label}. Line number must be ${numbers_js_1.LineLabelNumber.description}`, context);
    }
    return {
        ...context,
        type: "GOTO",
        destinationLabel: label,
    };
}
exports.parseGOTO = parseGOTO;
exports.MAX_GOSUB_ARG_COUNT = 15;
function parseGOSUB(tokens, context) {
    consumeExpected(tokens, "GOSUB", context);
    const { number: label } = consumeExpected(tokens, "number", context);
    if (!numbers_js_1.LineLabelNumber.isValid(label)) {
        throw (0, errors_js_1.parsingError)(`Invalid GOSUB: ${label}. Line number must be ${numbers_js_1.LineLabelNumber.description}`, context);
    }
    const args = [];
    while (!tokens.isDone() && tokens.peek()?.type === "name") {
        if (args.length >= exports.MAX_GOSUB_ARG_COUNT) {
            throw (0, errors_js_1.parsingError)(`GOSUB supports at most ${exports.MAX_GOSUB_ARG_COUNT} arguments`, context);
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
exports.parseGOSUB = parseGOSUB;
function parseExpression(tokens, context, initialLHS = null) {
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
exports.parseExpression = parseExpression;
function parseExpressionLHS(tokens, context) {
    const first = tokens.peek();
    switch (first?.type) {
        case "name":
            if (tokens.peek(1)?.type === "(") {
                return parseCallExpression(tokens, context);
            }
            else {
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
    throw (0, errors_js_1.unexpectedTokenError)({
        actual: first,
    }, context);
}
function parseExpressionRHS(tokens, opToken, context) {
    const next = tokens.peek();
    const nextNext = tokens.peek(1);
    if (
    // Prioritize parenthesized expressions
    next?.type === "(" ||
        // Defer to higher precedence opertators
        (nextNext?.type === "ibop" && nextNext.precedence < opToken.precedence)) {
        return parseExpression(tokens, context);
    }
    else {
        return parseExpressionLHS(tokens, context);
    }
}
function parseCallExpression(tokens, context) {
    const functionNameToken = consumeExpected(tokens, "name", context);
    consumeExpected(tokens, "(", context);
    const arg = parseExpression(tokens, context);
    consumeExpected(tokens, ")", context);
    return {
        type: "call",
        functionName: functionNameToken.name,
        arg,
    };
}
exports.parseCallExpression = parseCallExpression;
function parseLiteral(tokens, context) {
    const next = tokens.next().value;
    switch (next?.type) {
        case "number":
            return {
                type: "literal",
                token: next,
            };
    }
    throw (0, errors_js_1.unexpectedTokenError)({ actual: next }, context);
}
exports.parseLiteral = parseLiteral;
function parseReference(tokens, context) {
    const nameToken = consumeExpected(tokens, "name", context);
    return {
        type: "reference",
        identifier: nameToken.name,
    };
}
exports.parseReference = parseReference;
function parseReferenceIdentifier(identifier, _context) {
    if (identifier in reserved_js_1.STATUS_NAMES) {
        return {
            type: "status",
            name: identifier,
        };
    }
    if (reserved_js_1.ResidentPointSet.has(identifier)) {
        return {
            type: "residentPoint",
            name: identifier,
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
exports.parseReferenceIdentifier = parseReferenceIdentifier;
//
// Utilities
//
function consumeExpected(tokens, expected, context, expectedLabel = "") {
    const { done: missing, value: token } = tokens.next();
    if (missing || token.type !== expected) {
        throw (0, errors_js_1.unexpectedTokenError)({
            expected: expectedLabel || expected,
            actual: token,
        }, context);
    }
    return token;
}
//# sourceMappingURL=parser.js.map