export const SYMBOLS = ["(", ")", ",", "="];
export const KEYWORDS = [
    "IF",
    "THEN",
    "ELSE",
    "GOTO",
    "GOSUB",
    "RETURN",
];
export const KeywordSet = new Set(KEYWORDS);
// Values are precedence, lower wins
export const INFIX_BINARY_OPERATORS = {
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
export const RESIDENT_POINTS = [
    "$BATT",
    "DAY",
    "DAYOFM",
    "TIME",
    "CRTIME",
    "MONTH",
];
export const ResidentPointSet = new Set(RESIDENT_POINTS);
export const SECONDS_COUNTER_REGEX = /^SECND[S1-7]$/;
export const STATUS_NAMES = {
    DEAD: 0,
    LOW: 50,
    OK: 100,
};
//# sourceMappingURL=reserved.js.map