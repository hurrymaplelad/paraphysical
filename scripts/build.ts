import { build, emptyDir } from "https://deno.land/x/dnt@0.31.0/mod.ts";
import { copy } from "https://deno.land/std@0.159.0/fs/mod.ts";

const outDir = "./npm";

await emptyDir(outDir);

await Promise.allSettled([
  Deno.copyFile("README.md", "npm/README.md"),
  copy("examples", "npm/esm/examples"),
  copy("examples", "npm/script/examples"),
]);

await build({
  entryPoints: ["./main.ts"],
  outDir,
  shims: {
    deno: "dev",
  },
  package: {
    // package.json properties
    name: "paraphysical",
    version: Deno.args[0]?.replace(/^v/, ""),
    description: "PPCL interpreter",
    license: "MIT",
    repository: {
      type: "git",
      url: "git+https://github.com/hurrymaplelad/paraphysical.git",
    },
  }
});