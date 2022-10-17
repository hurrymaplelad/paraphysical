"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.assertNever = exports.fileNotLoaded = exports.invalidStatementError = exports.unexpectedTokenError = exports.parsingError = exports.runtimeError = exports.errorAddress = exports.printToken = void 0;
function printToken(token) {
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
exports.printToken = printToken;
function errorAddress({ filename, sourceLineNumber }) {
    return `[${filename}:${sourceLineNumber}]`;
}
exports.errorAddress = errorAddress;
function runtimeError(message, context) {
    return new Error(`Runtime Error: ${message} ${errorAddress(context)}`);
}
exports.runtimeError = runtimeError;
function parsingError(message, context) {
    return new Error(`Parse Error: ${message} ${errorAddress(context)}`);
}
exports.parsingError = parsingError;
function unexpectedTokenError({ expected, actual }, context) {
    const expectation = expected ? `Expected ${expected}. Got ` : "";
    return parsingError(`Unexepected token: ${expectation}${printToken(actual)}`, context);
}
exports.unexpectedTokenError = unexpectedTokenError;
function invalidStatementError({ outer, inner }, context) {
    return parsingError(`no ${inner} statements inside ${outer}s`, context);
}
exports.invalidStatementError = invalidStatementError;
function fileNotLoaded(filename) {
    return new Error(`Cannot run "${filename}" - not loaded.`);
}
exports.fileNotLoaded = fileNotLoaded;
function assertNever(_) { }
exports.assertNever = assertNever;
//# sourceMappingURL=errors.js.map