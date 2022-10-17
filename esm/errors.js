export function printToken(token) {
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
export function errorAddress({ filename, sourceLineNumber }) {
    return `[${filename}:${sourceLineNumber}]`;
}
export function runtimeError(message, context) {
    return new Error(`Runtime Error: ${message} ${errorAddress(context)}`);
}
export function parsingError(message, context) {
    return new Error(`Parse Error: ${message} ${errorAddress(context)}`);
}
export function unexpectedTokenError({ expected, actual }, context) {
    const expectation = expected ? `Expected ${expected}. Got ` : "";
    return parsingError(`Unexepected token: ${expectation}${printToken(actual)}`, context);
}
export function invalidStatementError({ outer, inner }, context) {
    return parsingError(`no ${inner} statements inside ${outer}s`, context);
}
export function fileNotLoaded(filename) {
    return new Error(`Cannot run "${filename}" - not loaded.`);
}
export function assertNever(_) { }
//# sourceMappingURL=errors.js.map