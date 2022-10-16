import { expect } from "https://deno.land/x/expect@v0.2.10/mod.ts";
import { assert } from "https://deno.land/std@0.97.0/testing/asserts.ts";
import { parseLine } from "../src/parser.ts";

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
    const statement = parseLine("00001 foo = 1", context);
    assert(statement.type === "assignment");
  });

  await t.step("Binary Operators", () => {
    { // Higher precedence is respected
      const statement = parseLine("001  X = 2 + 3 * 4", context);
      assert(statement.type === "assignment");
      const { rhs: exp } = statement;
      assert(exp.type === "ibop");
      assert(exp.operator === "+");
      assert(exp.rhs.type === "ibop");
      assert(exp.rhs.lhs.type === "literal");
      assert(exp.rhs.lhs.token.number === 3);
    }
    { // Left-to-right for equal precedence
      const statement = parseLine("001  X = 2 + 3 + 4", context);
      assert(statement.type === "assignment");
      const { rhs: exp } = statement;
      assert(exp.type === "ibop");
      assert(exp.operator === "+");
      assert(exp.lhs.type === "ibop");
      assert(exp.rhs.type === "literal");
      assert(exp.rhs.token.number === 4);
    }
    { // Parens win
      const statement = parseLine("001  X = 2 + (3 + 4)", context);
      assert(statement.type === "assignment");
      const { rhs: exp } = statement;
      assert(exp.type === "ibop");
      assert(exp.operator === "+");
      assert(exp.lhs.type === "literal");
      assert(exp.rhs.type === "ibop");
    }
  });

  await t.step("Conditional Statements", () => {
    const statement = parseLine("1 IF(X) THEN Y = Z ELSE ON(Z)", context);
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
