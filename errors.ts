export type LineContext = Readonly<{
  filename: string;
  // The implicit line number in source code, not the authored line number prefix
  sourceLineNumber: number;
}>;

export function makeParseError(
  message: string,
  { filename, sourceLineNumber }: LineContext,
): Error {
  return new Error(
    `Parse Error: ${message} [${filename}:${sourceLineNumber}]`,
  );
}
