export const SYMBOLS = ["(", ")", ",", "="] as const;
export type SymbolsType = typeof SYMBOLS[number];

export const KEYWORDS = [
  "IF",
  "THEN",
  "ELSE",
  "GOTO",
  "GOSUB",
  "RETURN",
] as const;
export type KeywordsType = typeof KEYWORDS[number];
export const KeywordSet: Set<string> = new Set(KEYWORDS);

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
} as const;
export type InfixBinaryOperatorType = keyof typeof INFIX_BINARY_OPERATORS;

export const RESIDENT_POINTS = [
  "$BATT",
  "DAY",
  "DAYOFM",
  "TIME",
  "CRTIME",
  "MONTH",
] as const;
export type ResidentPointNameType = typeof RESIDENT_POINTS[number];
export const ResidentPointSet = new Set(RESIDENT_POINTS);

export const STATUS_NAMES = {
  DEAD: 0,
  LOW: 50,
  OK: 100,
} as const;
export type StatusNameType = keyof typeof STATUS_NAMES;
