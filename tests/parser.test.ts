import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";
import { assert } from "https://deno.land/std@0.97.0/testing/asserts.ts";
import { parseLine } from "../parser.ts";

Deno.test("parseLine()", async (t) => {
  const context = { filename: "test.ppcl", sourceLineNumber: 0 };

  await t.step("Call Statements", () => {
    let statement;

    // Simple
    statement = parseLine("1 foo(bar)", context);
    assert(statement.type === "call");
    expect(statement.functionName).toEqual("foo");
    expect(statement.args).toEqual([{ type: "reference", identifier: "bar" }]);

    // Multiple args
    statement = parseLine("00001 foo(bar, baz)", context);
    assert(statement.type === "call");
    expect(statement.functionName).toEqual("foo");
    expect(statement.args).toEqual([
      { type: "reference", identifier: "bar" },
      { type: "reference", identifier: "baz" },
    ]);
  });

  await t.step("Assignment Statements", () => {
    let statement;

    // Simple
    statement = parseLine("00001 foo = 1", context);
    assert(statement.type === "assignment");
  });

  await t.step("Conditional Statements", () => {
    let statement;
    statement = parseLine("1 IF(X) THEN Y = Z ELSE ON(Z)", context);
    assert(statement.type === "conditional");
    assert(statement.condition.type === "reference");
    assert(statement.then.type === "assignment");
    assert(statement.else?.type === "call");
  });

  await t.step("Invalid Statements", () => {
    // Trailing characters
    expect(() => parseLine("00001 foo = 1 a", context)).toThrow(/Parse Error/);
  });
});
