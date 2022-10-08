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

  await t.step("Parens", () => {
    const tokens = tokenizeLine("A( (B),))", context);
    expect(tokens).toEqual([
      {
        type: "name",
        name: "A",
      },
      { type: "(" },
      { type: "(" },
      {
        type: "name",
        name: "B",
      },
      { type: ")" },
      { type: "," },
      { type: ")" },
      { type: ")" },
    ]);
  });

  await t.step("Conditionals", () => {
    const tokens = tokenizeLine("IF(X) THEN Y = Z ELSE ON(Z)", context);
    expect(tokens).toEqual([
      {
        type: "IF",
      },
      { type: "(" },
      {
        type: "name",
        name: "X",
      },
      { type: ")" },
      { type: "THEN" },
      {
        type: "name",
        name: "Y",
      },
      { type: "=" },
      {
        type: "name",
        name: "Z",
      },
      { type: "ELSE" },
      {
        type: "name",
        name: "ON",
      },
      { type: "(" },
      {
        type: "name",
        name: "Z",
      },
      { type: ")" },
    ]);
  });

  await t.step("Binary Operators", () => {
    const tokens = tokenizeLine("(AB + C).EQ.9.0", context);
    expect(tokens).toEqual([
      { type: "(" },
      {
        type: "name",
        name: "AB",
      },
      {
        type: "ibop",
        operator: "+",
        precedence: 5,
      },
      {
        type: "name",
        name: "C",
      },
      { type: ")" },
      {
        type: "ibop",
        operator: ".EQ.",
        precedence: 6,
      },
      {
        type: "number",
        number: 9.0,
      },
    ]);
  });
});
