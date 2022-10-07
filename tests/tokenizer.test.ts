import { tokenizeLine } from "../interpreter.ts";
import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";

Deno.test("tokenizeLine()", async (t) => {
  const context = { filename: "test.ppcl", sourceLineNumber: 0 };

  await t.step("numbers", () => {
    let tokens;
    // Integer
    tokens = tokenizeLine("10", context);
    expect(tokens).toEqual([{
      type: "number",
      number: 10,
    }]);

    // Decimal
    tokens = tokenizeLine("99.989", context);
    expect(tokens).toEqual([{
      type: "number",
      number: 99.989,
    }]);
  });
});
