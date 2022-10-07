import Interpreter from "../interpreter.ts";
import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";

async function readExampleText(filename: string): Promise<string> {
  return await Deno.readTextFile(`examples/${filename}`);
}

Deno.test("Interpreter", async () => {
  const interpreter = new Interpreter();
  const filename = "hello.ppcl";
  interpreter.load(filename, await readExampleText(filename));
  interpreter.run(filename);

  expect(interpreter.getLocal("out")).toEqual(1);
});
