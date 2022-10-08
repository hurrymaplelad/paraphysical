import { Interpreter } from "../interpreter.ts";
import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";

async function readExampleText(filename: string): Promise<string> {
  return await Deno.readTextFile(`examples/${filename}`);
}

function inlineExample(content: string): string {
  return content.split("\n").map((l) => l.trim()).filter(Boolean).join("\n");
}

Deno.test("Interpreter", async (t) => {
  const context = { filename: "test", sourceLineNumber: NaN };

  await t.step("Hello World", async () => {
    const interpreter = new Interpreter();
    const filename = "hello.ppcl";
    interpreter.load(filename, await readExampleText(filename));
    interpreter.run(filename);

    expect(interpreter.getLocal("OUT", context)).toEqual(1);
    expect(interpreter.getPoint("HELLO.WORLD", context)).toEqual(2);
  });

  await t.step("Binary Expressions", () => {
    const interpreter = new Interpreter();
    const filename = "binex.ppcl";
    const content = inlineExample(`
      001  X = 2 + 3 * 4
    `);
    interpreter.load(filename, content);
    interpreter.run(filename);

    expect(interpreter.getPoint("X", context)).toEqual(14);
  });

  await t.step("Conditionals", () => {
    const interpreter = new Interpreter();
    const filename = "conditional.ppcl";
    const content = inlineExample(`
      1 X = 1
      2 Y = 1
      3 IF(X) THEN Y = 0
      4 IF(Y) THEN X = 0 ELSE X = 2
    `);
    interpreter.load(filename, content);
    interpreter.run(filename);

    expect(interpreter.getPoint("X", context)).toEqual(2);
    expect(interpreter.getPoint("Y", context)).toEqual(0);
  });

  await t.step("GOTO", () => {
    const interpreter = new Interpreter();
    const filename = "goto.ppcl";
    const content = inlineExample(`
      001  X = 0
      002  X = X + 1
      003  C| This example only makes sense in our interpreter,
      004  C| where we can run the program only once. 
      100  IF(X.LT.3) THEN GOTO 2
    `);
    interpreter.load(filename, content);
    interpreter.run(filename);

    expect(interpreter.getPoint("X", context)).toEqual(3);
  });
});
