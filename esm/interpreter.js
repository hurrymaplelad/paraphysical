var __classPrivateFieldSet = (this && this.__classPrivateFieldSet) || function (receiver, state, value, kind, f) {
    if (kind === "m") throw new TypeError("Private method is not writable");
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a setter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
    return (kind === "a" ? f.call(receiver, value) : f ? f.value = value : state.set(receiver, value)), value;
};
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _Interpreter_instances, _Interpreter_files, _Interpreter_points, _Interpreter_currentFilename, _Interpreter_currentFileState, _Interpreter_statementState, _Interpreter_keyLocal, _Interpreter_isLocalDeclared;
import { SystemClock } from "./clocks.js";
import { assertNever, fileNotLoaded, runtimeError, } from "./errors.js";
import { parseFile, parseReferenceIdentifier, } from "./parser.js";
import { LineLabelNumber } from "./numbers.js";
import { SECONDS_COUNTER_REGEX, STATUS_NAMES, } from "./reserved.js";
import { DateTime } from "./datetime.js";
const LOCAL_DELIMITER = ":";
const MAX_GOSUB_STACK_DEPTH = 8;
const DEFAULT_STATEMENT_STATES = {
    SAMPLE: () => ({
        type: "SAMPLE",
        lastRunTimestamp: NaN,
    }),
};
const DEFAULT_RESIDENT_POINT_VALUES = {
    "$BATT": STATUS_NAMES.OK,
};
export class Interpreter {
    constructor(options = null) {
        _Interpreter_instances.add(this);
        _Interpreter_files.set(this, void 0);
        // Key is filename:variablen. Use getLocal() / setLocal()
        _Interpreter_points.set(this, void 0);
        _Interpreter_currentFilename.set(this, "");
        Object.defineProperty(this, "clock", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "timezone", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: "America/Los_Angeles"
        }); // IANA zone name
        __classPrivateFieldSet(this, _Interpreter_files, new Map(), "f");
        __classPrivateFieldSet(this, _Interpreter_points, new Map(), "f");
        this.clock = options?.clock ?? new SystemClock();
        if (options?.timezone != null) {
            this.timezone = options.timezone;
        }
        // Initialize resident points
        for (const [name, value] of Object.entries(DEFAULT_RESIDENT_POINT_VALUES)) {
            __classPrivateFieldGet(this, _Interpreter_points, "f").set(name, value);
        }
    }
    load(filename, contents) {
        const parsedFile = parseFile(contents, { filename });
        const state = {
            disabledLines: new Set(),
            gosubStack: [],
            locals: new Map(),
            programCounter: 0,
            statementStates: new Map(),
            timestampAtStartOfLatestRun: NaN,
            secondsCounterAssignmentTimestamps: new Map(),
        };
        __classPrivateFieldGet(this, _Interpreter_files, "f").set(filename, [parsedFile, state]);
    }
    dateTime() {
        return new DateTime(this.clock.getTimestamp(), this.timezone);
    }
    /**
     * Run the argument file until the first time the final line
     * executes. Yields after each line.
     *
     * Program state will remain in this interpreter. For example,
     * if the final statement was a GOTO, the programCounter will point
     * to the GOTO line if the file is run again.
     */
    *runOnce(filename) {
        const record = __classPrivateFieldGet(this, _Interpreter_files, "f").get(filename);
        if (record == null) {
            throw fileNotLoaded(filename);
        }
        const [{ statements, maxLabel }, state] = record;
        __classPrivateFieldSet(this, _Interpreter_currentFilename, filename, "f");
        state.timestampAtStartOfLatestRun = this.clock.getTimestamp();
        while (state.programCounter <= maxLabel) {
            const statement = statements.get(state.programCounter);
            // It's important to increment the program counter *before* evaluating
            // the statement in case it's a GOTO or similar.
            state.programCounter += 1;
            // if (statement != null) {
            //   console.log("evaluating", statement.label, statement.type);
            // }
            if (statement != null && !state.disabledLines.has(statement.label)) {
                this.evaluateStatement(statement);
            }
            // Check if we reached the end of the file, which triggers
            // time increments and such.
            if (statement?.label === maxLabel) {
                break;
            }
            yield;
        }
    }
    runOnceSync(filename) {
        for (const _ of this.runOnce(filename)) {
            // consume line
        }
    }
    evaluateStatement(statement) {
        switch (statement.type) {
            case "assignment":
                return this.evaluateAssignment(statement);
            case "call":
                return this.evaluateCallStatement(statement);
            case "comment":
                return;
            case "conditional":
                return this.evaluateConditional(statement);
            case "GOTO":
                return this.evaluateGOTO(statement);
            case "GOSUB":
                return this.evaluateGOSUB(statement);
            case "RETURN":
                return this.evaluateRETURN(statement);
            case "SAMPLE":
                return this.evaluateSAMPLE(statement);
            default:
                // Enforce exhaustiveness
                assertNever(statement);
        }
    }
    evaluateConditional(statement) {
        const conditionValue = this.evaluateExpression(statement.condition, statement);
        if (conditionValue) {
            this.evaluateStatement(statement.then);
        }
        else if (statement.else != null) {
            this.evaluateStatement(statement.else);
        }
    }
    evaluateAssignment(statement) {
        const { lhs, rhs } = statement;
        const value = this.evaluateExpression(rhs, statement);
        const dest = parseReferenceIdentifier(lhs.identifier, statement);
        switch (dest.type) {
            case "local":
                return this.setLocal(dest.keyOrName, value, statement);
            case "point":
                if (SECONDS_COUNTER_REGEX.test(dest.name)) {
                    return this.setSecondsCounter(dest.name, value, statement);
                }
                return this.setPoint(dest.name, value);
        }
        throw runtimeError(`cannot assign to ${lhs.identifier}`, statement);
    }
    evaluateGOTO(statement) {
        const { destinationLabel } = statement;
        __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement).programCounter = destinationLabel;
    }
    evaluateGOSUB(statement) {
        const fileState = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement);
        const { args, destinationLabel } = statement;
        if (fileState.gosubStack.length >= MAX_GOSUB_STACK_DEPTH) {
            throw runtimeError(`Maximum of ${MAX_GOSUB_STACK_DEPTH} nested GOSUB calls exceeded`, statement);
        }
        fileState.gosubStack.push(statement.label);
        for (const [index, arg] of args.entries()) {
            const argValue = this.evaluateExpression(arg, statement);
            // Avoid setLocal() to bypass declaration check
            __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement).locals.set("ARG" + (index + 1), argValue);
        }
        fileState.programCounter = destinationLabel;
    }
    evaluateRETURN(statement) {
        const fileState = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement);
        const callsiteLabel = fileState.gosubStack.pop();
        if (callsiteLabel == undefined) {
            throw runtimeError("RETURN outside GOSUB", statement);
        }
        fileState.programCounter = callsiteLabel + 1;
    }
    evaluateSAMPLE(statement) {
        const { sampledStatement, secondsPerSample } = statement;
        const fileState = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement);
        const statementState = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_statementState).call(this, statement);
        const { lastRunTimestamp } = statementState;
        const currentTime = fileState.timestampAtStartOfLatestRun;
        if (isNaN(lastRunTimestamp) ||
            currentTime - lastRunTimestamp >= secondsPerSample) {
            statementState.lastRunTimestamp = currentTime;
            this.evaluateStatement(sampledStatement);
        }
    }
    evaluateCallStatement(statement) {
        const { functionName, args } = statement;
        switch (functionName.toUpperCase()) {
            case "LOCAL":
                return this.evaluateLOCAL(args, statement);
            case "ENABLE":
            case "ACT":
                return this.evaluateENABLE(args, statement);
            case "DISABLE":
            case "DEACT":
                return this.evaluateDISABLE(args, statement);
        }
        throw runtimeError(`unrecognized function: ${functionName}`, statement);
    }
    evaluateLOCAL(args, context) {
        for (const arg of args) {
            if (arg.type !== "reference") {
                throw runtimeError(`invalid LOCAL declaration: ${arg.type}`, context);
            }
            const name = arg.identifier;
            if (name.indexOf(LOCAL_DELIMITER) > -1) {
                throw runtimeError(`invalid local declaration: ${name}`, context);
            }
            const key = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_keyLocal).call(this, name, context);
            if (__classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_isLocalDeclared).call(this, key)) {
                throw runtimeError(`attemped to re-declare LOCAL: ${name}`, context);
            }
            __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, context).locals.set(name, 0);
        }
    }
    evaluateENABLE(args, context) {
        for (const arg of args) {
            if (arg.type !== "literal" || !LineLabelNumber.isValid(arg.token.number)) {
                throw runtimeError(`ENABLE/ACT arg must be ${LineLabelNumber.description}`, context);
            }
            const lineLabel = arg.token.number;
            __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, context).disabledLines.delete(lineLabel);
        }
    }
    evaluateDISABLE(args, context) {
        for (const arg of args) {
            if (arg.type !== "literal" || !LineLabelNumber.isValid(arg.token.number)) {
                throw runtimeError(`DISABLE/DEACT arg must be ${LineLabelNumber.description}`, context);
            }
            const lineLabel = arg.token.number;
            __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, context).disabledLines.add(lineLabel);
        }
    }
    getLocal(nameOrKey, context) {
        const [filename, name] = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_keyLocal).call(this, nameOrKey, context);
        const value = __classPrivateFieldGet(this, _Interpreter_files, "f").get(filename)?.[1].locals.get(name);
        if (value === undefined) {
            throw runtimeError(`attempted to read undeclared LOCAL: ${nameOrKey}`, context);
        }
        return value;
    }
    setLocal(nameOrKey, value, context) {
        const key = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_keyLocal).call(this, nameOrKey, context);
        const [filename, name] = key;
        const fileState = __classPrivateFieldGet(this, _Interpreter_files, "f").get(filename)?.[1];
        if (fileState == null || !__classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_isLocalDeclared).call(this, key)) {
            throw runtimeError(`attempted to set undeclared LOCAL: ${key}`, context);
        }
        fileState.locals.set(name, value);
    }
    getPoint(name, context) {
        const value = __classPrivateFieldGet(this, _Interpreter_points, "f").get(name);
        if (value == null) {
            throw runtimeError(`point ${name} is not defined`, context);
        }
        return value;
    }
    setPoint(name, value) {
        __classPrivateFieldGet(this, _Interpreter_points, "f").set(name, value);
    }
    getSecondsCounter(name, context) {
        const assignmentTime = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, context)
            .secondsCounterAssignmentTimestamps.get(name) ??
            this.clock.initialTimestamp();
        const assignedCount = __classPrivateFieldGet(this, _Interpreter_points, "f").get(name) ?? 0;
        return Math.floor(this.clock.getTimestamp() - assignmentTime + assignedCount);
    }
    setSecondsCounter(name, value, context) {
        const assignmentTime = this.clock.getTimestamp();
        __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, context).secondsCounterAssignmentTimestamps.set(name, assignmentTime);
        __classPrivateFieldGet(this, _Interpreter_points, "f").set(name, value);
    }
    getResidentPoint(name, content) {
        switch (name) {
            case "DAY":
                return this.dateTime().weekdayNumber();
            case "DAYOFM":
                return this.dateTime().dayOfMonth();
            case "CRTIME":
            case "TIME": // TODO - enforce that TIME type is not assignable
                return this.dateTime().fractionalHour();
            case "MONTH":
                return this.dateTime().monthNumber();
        }
        return this.getPoint(name, content);
    }
    evaluateExpression(expression, context) {
        switch (expression.type) {
            case "literal":
                return this.evaluateLiteralExpression(expression, context);
            case "reference":
                return this.evaluateReferenceExpression(expression, context);
            case "ibop":
                return this.evaluateInfixBinaryOperation(expression, context);
            case "call":
                return this.evaluateCallExpression(expression, context);
        }
    }
    evaluateLiteralExpression(expression, _context) {
        const token = expression.token;
        switch (token.type) {
            case "number":
                return token.number;
        }
    }
    evaluateReferenceExpression(expression, context) {
        const id = parseReferenceIdentifier(expression.identifier, context);
        switch (id.type) {
            case "local":
                return this.getLocal(id.keyOrName, context);
            case "point":
                if (SECONDS_COUNTER_REGEX.test(id.name)) {
                    return this.getSecondsCounter(id.name, context);
                }
                return this.getPoint(id.name, context);
            case "residentPoint":
                return this.getResidentPoint(id.name, context);
            case "status":
                return STATUS_NAMES[id.name];
        }
    }
    evaluateInfixBinaryOperation(expression, context) {
        const lhsValue = this.evaluateExpression(expression.lhs, context);
        const rhsValue = () => this.evaluateExpression(expression.rhs, context);
        switch (expression.operator) {
            // Arithmetic
            case "+":
                return lhsValue + rhsValue();
            case "-":
                return lhsValue - rhsValue();
            case "*":
                return lhsValue * rhsValue();
            case "/":
                return lhsValue / rhsValue();
            case ".ROOT.":
                return Math.pow(lhsValue, 1 / rhsValue());
            // Comparison
            case ".EQ.":
                return Number(lhsValue === rhsValue());
            case ".NE.":
                return Number(lhsValue !== rhsValue());
            case ".GT.":
                return Number(lhsValue > rhsValue());
            case ".GE.":
                return Number(lhsValue >= rhsValue());
            case ".LT.":
                return Number(lhsValue < rhsValue());
            case ".LE.":
                return Number(lhsValue <= rhsValue());
            // Logical
            case ".AND.":
                return Number(lhsValue && rhsValue());
            case ".NAND.":
                return Number(!(lhsValue && rhsValue()));
            case ".OR.":
                return Number(lhsValue || rhsValue());
            case ".XOR.": {
                const rhsVal = rhsValue();
                return Number((lhsValue || rhsVal) && !(lhsValue && rhsVal));
            }
            default:
                assertNever(expression.operator);
                throw runtimeError(`operator ${expression.operator} not implemented`, context);
        }
    }
    evaluateCallExpression(expression, context) {
        const { functionName, arg } = expression;
        const argValue = this.evaluateExpression(arg, context);
        switch (functionName) {
            case "ATN":
                return Math.atan(argValue);
            case "COM":
                return 1 - (argValue ? 1 : 0);
            case "COS":
                return Math.cos(argValue);
            case "EXP":
                return Math.exp(argValue);
            case "LOG":
                return Math.log(argValue);
            case "SIN":
                return Math.sin(argValue);
            case "SQRT":
                return Math.sqrt(argValue);
            case "TAN":
                return Math.tan(argValue);
        }
        throw runtimeError(`unrecognized function: ${functionName}`, context);
    }
}
_Interpreter_files = new WeakMap(), _Interpreter_points = new WeakMap(), _Interpreter_currentFilename = new WeakMap(), _Interpreter_instances = new WeakSet(), _Interpreter_currentFileState = function _Interpreter_currentFileState(context) {
    const record = __classPrivateFieldGet(this, _Interpreter_files, "f").get(__classPrivateFieldGet(this, _Interpreter_currentFilename, "f"));
    if (record == null) {
        throw runtimeError("No running file", context);
    }
    return record[1];
}, _Interpreter_statementState = function _Interpreter_statementState(statement) {
    const { statementStates } = __classPrivateFieldGet(this, _Interpreter_instances, "m", _Interpreter_currentFileState).call(this, statement);
    const { label } = statement;
    if (statementStates.has(label)) {
        return statementStates.get(label);
    }
    else {
        const state = DEFAULT_STATEMENT_STATES[statement.type]();
        statementStates.set(label, state);
        return state;
    }
}, _Interpreter_keyLocal = function _Interpreter_keyLocal(nameOrKey, context) {
    const parts = nameOrKey.split(LOCAL_DELIMITER);
    switch (parts.length) {
        case 1:
            return [__classPrivateFieldGet(this, _Interpreter_currentFilename, "f"), nameOrKey];
        case 2:
            return [parts[0], parts[1]];
        default:
            throw runtimeError(`invalid local: ${nameOrKey}`, context);
    }
}, _Interpreter_isLocalDeclared = function _Interpreter_isLocalDeclared([filename, name]) {
    return __classPrivateFieldGet(this, _Interpreter_files, "f").get(filename)?.[1].locals.has(name) ?? false;
};
//# sourceMappingURL=interpreter.js.map