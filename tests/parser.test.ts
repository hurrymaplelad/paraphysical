import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";
import { assert } from "https://deno.land/std@0.97.0/testing/asserts.ts";
import { parseLine } from "../parser.ts";

Deno.test("parseLine()", async (t) => {
  const context = { filename: "test.ppcl", sourceLineNumber: 0 };
  await t.step("call statement", () => {
    let statement;

    // Simple
    statement = parseLine("00001 foo(bar)", context);
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
});
