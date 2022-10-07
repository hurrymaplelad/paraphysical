import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";
import { tokenizeLine } from "../tokenizer.ts";

Deno.test("tokenizeLine()", async (t) => {
  const context = { filename: "test.ppcl", sourceLineNumber: 0 };

  await t.step("Numbers", () => {
    let tokens;
    // Integer
    tokens = tokenizeLine("10", context);
    expect(tokens).toEqual([{
      type: "number",
      number: 10,
    }]);

    // Decimal
    tokens = tokenizeLine(" 99.989 ", context);
    expect(tokens).toEqual([{
      type: "number",
      number: 99.989,
    }]);
  });

  await t.step("Names", () => {
    let tokens;

    // Simple
    tokens = tokenizeLine("boop ", context);
    expect(tokens).toEqual([{
      type: "name",
      name: "boop",
    }]);

    // Quoted
    tokens = tokenizeLine('"$boop.-5 A"', context);
    expect(tokens).toEqual([{
      type: "name",
      name: "$boop.-5 A",
    }]);
  });
});
