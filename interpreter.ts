type Statement = Readonly<
  & {
    // The authored line number at the start of each line
    label: number;
    // The implicit line number in source code, not the authored line number prefix
    sourceLineNumber: number;
    filename: string;
  }
  & (
    {
      type: "comment";
      comment: string;
    } | {
      type: "assignment";
    }
  )
>;

function makeParseError(
  message: string,
  { filename, sourceLineNumber }: Readonly<
    { filename: string; sourceLineNumber: number }
  >,
): Error {
  return new Error(
    `Parse Error: ${message} [${filename}:${sourceLineNumber}]`,
  );
}

function parseLine(
  line: string,
  context: Readonly<{ filename: string; sourceLineNumber: number }>,
): Statement {
  // Line label
  const match = /^([0-9]{5})\s+(.*)$/.exec(line);
  if (match == null) {
    throw makeParseError("Line must start with 5 digit label", context);
  }
  const [_, labelString, rest] = match;
  const label = parseInt(labelString, 10);

  return { type: "comment", comment: rest, label, ...context };
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
