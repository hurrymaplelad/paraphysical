import { Interpreter } from "../interpreter.ts";
import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";
import { ManualClock } from "../clocks.ts";
import { range } from "../numbers.ts";

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
    interpreter.runOnceSync(filename);

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
    interpreter.runOnceSync(filename);

    expect(interpreter.getPoint("X", context)).toEqual(14);
  });

  await t.step("Arithmetic Functions", () => {
    const interpreter = new Interpreter();
    const filename = "arithmetic.ppcl";
    const content = inlineExample(`
      001  X = SQRT(4)
    `);
    interpreter.load(filename, content);
    interpreter.runOnceSync(filename);

    expect(interpreter.getPoint("X", context)).toEqual(2);
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
    interpreter.runOnceSync(filename);

    expect(interpreter.getPoint("X", context)).toEqual(2);
    expect(interpreter.getPoint("Y", context)).toEqual(0);
  });

  await t.step("GOTO", () => {
    const interpreter = new Interpreter();
    const filename = "goto.ppcl";
    const content = inlineExample(`
      001  X = 0
      002  X = X + 1
      003  C
      100  IF(X.LT.3) THEN GOTO 2 ELSE GOTO 3
    `);
    interpreter.load(filename, content);
    for (const _ of range({ end: 5 })) {
      interpreter.runOnceSync(filename);
    }

    expect(interpreter.getPoint("X", context)).toEqual(3);
  });

  await t.step("GOSUB", () => {
    const interpreter = new Interpreter();
    const filename = "gosub.ppcl";
    const content = inlineExample(`
      001  C == Main Loop ==
      002  X = 1
      003  GOSUB 111 X
      004  GOTO 200
      005  C
      100  C == Subroutines ==
      110  C -- Sub 1 --
      111  X = X + $ARG1
      112  GOSUB 121 $ARG1 X
      113  RETURN
      120  C -- Sub 2 --
      121  X = X + $ARG2
      122  RETURN
      200  GOTO 1
    `);
    interpreter.load(filename, content);
    interpreter.runOnceSync(filename);

    expect(interpreter.getPoint("X", context)).toEqual(4);
  });

  await t.step("SAMPLE", () => {
    const clock = new ManualClock();
    const interpreter = new Interpreter({ clock });
    const filename = "sample.ppcl";
    const content = inlineExample(`
      001  X = 0
      002  SAMPLE(3) X = X + 1
      003  GOTO 2
    `);
    interpreter.load(filename, content);
    for (const _ of range({ end: 8 })) {
      interpreter.runOnceSync(filename);
      clock.tick(1);
    }

    // Should increment on run 1, 4, and 7
    expect(interpreter.getPoint("X", context)).toEqual(3);
  });

  await t.step("ENABLE/ACT, DISABLE/DEACT", () => {
    const interpreter = new Interpreter();
    const filename = "enable.ppcl";
    const content = inlineExample(`
      001  X = 0
      002  DISABLE(4)
      003  ENABLE(4)
      004  X = 2
      005  DISABLE(6)
      006  X = 3
    `);
    interpreter.load(filename, content);
    interpreter.runOnceSync(filename);

    expect(interpreter.getPoint("X", context)).toEqual(2);
  });
});
