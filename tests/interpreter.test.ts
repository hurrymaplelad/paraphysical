import { Interpreter } from "../interpreter.ts";
import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";

async function readExampleText(filename: string): Promise<string> {
  return await Deno.readTextFile(`examples/${filename}`);
}

Deno.test("Interpreter", async (t) => {
  const interpreter = new Interpreter();

  //   await t.step("evaluate a simple file", async () => {
  //     const filename = "hello.ppcl";
  //     interpreter.load(filename, await readExampleText(filename));
  //     interpreter.run(filename);

  //     expect(interpreter.getLocal("out")).toEqual(1);
  //   });

  await t.step("throw parsing an invalid file", async () => {
    const filename = "invalid-line-number.ppcl";
    const contents = await readExampleText(filename);
    expect(() => interpreter.load(filename, contents)).toThrow(
      /Parse Error.*invalid-line-number.ppcl:2/,
    );
  });
});
