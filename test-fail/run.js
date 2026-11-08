// Compiles each malformed API in this directory against the library and
// checks that it fails with the custom type error named by its first line,
// `-- expect: <text>`.
//
// Run from the repository root with `npm run test:fail`.

import { spawnSync } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const dir = "test-fail";
const output = ".spago/fail-output";

const run = (cmd, args) => spawnSync(cmd, args, { encoding: "utf8" });

// The library's dependency globs plus `src/**`, without the test suite.
const sources = run("npx", ["spago", "sources", "-q", "-p", "windrose-router"]);
if (sources.status !== 0) {
  process.stderr.write(sources.stderr);
  process.exit(1);
}
const globs = sources.stdout
  .split("\n")
  .map((line) => line.trim())
  .filter((line) => line !== "" && !line.startsWith("test/"));

// Normalize the compiler's line wrapping before searching its output.
const squash = (text) => text.replace(/\s+/g, " ");

let failures = 0;
for (const file of readdirSync(dir).filter((f) => f.endsWith(".purs")).sort()) {
  const path = join(dir, file);
  const header = readFileSync(path, "utf8").split("\n")[0];
  const match = header.match(/^-- expect: (.+)$/);
  if (!match) {
    console.log(`FAIL ${file}: first line is not "-- expect: <text>"`);
    failures++;
    continue;
  }
  const expected = match[1].trim();
  const result = run("npx", ["purs", "compile", "--output", output, ...globs, path]);
  const text = result.stdout + result.stderr;
  if (result.status === 0) {
    console.log(`FAIL ${file}: compiled, but should have failed`);
    failures++;
  } else if (!squash(text).includes(squash(expected))) {
    console.log(`FAIL ${file}: error does not mention "${expected}"`);
    console.log(text);
    failures++;
  } else {
    console.log(`ok   ${file}`);
  }
}

if (failures > 0) {
  console.log(`${failures} compile-fail test(s) failed.`);
  process.exit(1);
}
console.log("All compile-fail tests passed.");
