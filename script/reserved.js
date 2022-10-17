"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.STATUS_NAMES = exports.SECONDS_COUNTER_REGEX = exports.ResidentPointSet = exports.RESIDENT_POINTS = exports.INFIX_BINARY_OPERATORS = exports.KeywordSet = exports.KEYWORDS = exports.SYMBOLS = void 0;
exports.SYMBOLS = ["(", ")", ",", "="];
exports.KEYWORDS = [
    "IF",
    "THEN",
    "ELSE",
    "GOTO",
    "GOSUB",
    "RETURN",
];
exports.KeywordSet = new Set(exports.KEYWORDS);
// Values are precedence, lower wins
exports.INFIX_BINARY_OPERATORS = {
    // 3
    ".ROOT.": 3,
    // 4
    "*": 4,
    "/": 4,
    // 5
    "+": 5,
    "-": 5,
    // 6
    ".EQ.": 6,
    ".NE.": 6,
    ".GT.": 6,
    ".GE.": 6,
    ".LT.": 6,
    ".LE.": 6,
    // 7
    ".AND.": 7,
    ".NAND.": 7,
    // 8
    ".OR.": 8,
    ".XOR.": 8,
};
exports.RESIDENT_POINTS = [
    "$BATT",
    "DAY",
    "DAYOFM",
    "TIME",
    "CRTIME",
    "MONTH",
];
exports.ResidentPointSet = new Set(exports.RESIDENT_POINTS);
exports.SECONDS_COUNTER_REGEX = /^SECND[S1-7]$/;
exports.STATUS_NAMES = {
    DEAD: 0,
    LOW: 50,
    OK: 100,
};
//# sourceMappingURL=reserved.js.map