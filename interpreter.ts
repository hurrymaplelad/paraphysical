import { LineContext, makeParseError } from "./errors.ts";

type Statement = Readonly<
  & LineContext
  & {
    // The authored line number at the start of each line
    label: number;
  }
  & (
    | {
      type: "comment";
      comment: string;
    }
    | {
      type: "call";
      functionName: string;
      args: ReadonlyArray<string>;
    }
    | {
      type: "assignment";
    }
  )
>;

function parseLine(
  line: string,
  context: LineContext,
): Statement {
  // Line label
  const labelMatch = /^([0-9]{5})\s+(.*)$/.exec(line);
  if (labelMatch == null) {
    throw makeParseError("Line must start with 5 digit label", context);
  }
  const [_, labelString, rest] = labelMatch;
  const label = parseInt(labelString, 10);

  // Comment
  if (rest.startsWith("C")) {
    return { type: "comment", comment: rest, label, ...context };
  }

  // Call
  const callMatch = /^(\w+)\((.*)\)\w*$/.exec(rest);
  if (callMatch) {
    const [_, functionName, argString] = callMatch;
    const args = argString.split(",").map((a) => a.trim());
    return { type: "call", functionName, args, label, ...context };
  }

  throw makeParseError("Unrecognized statement", context);
}

type ParsedFile = Map<number, Statement>;

function parseFile(
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

export class Interpreter {
  #files: { [key: string]: ParsedFile };

  constructor() {
    this.#files = {};
  }

  load(filename: string, contents: string): void {
    this.#files[filename] = parseFile(contents, { filename });
  }

  run(filename: string): void {
  }

  getLocal(name: string): unknown {
    return null;
  }
}
