import { ParsedFile, parseFile } from "./parser.ts";

export class Interpreter {
  #files: { [key: string]: ParsedFile };

  constructor() {
    this.#files = {};
  }

  load(filename: string, contents: string): void {
    this.#files[filename] = parseFile(contents, { filename });
  }

  run(_filename: string): void {
  }

  getLocal(_name: string): unknown {
    return null;
  }
}
