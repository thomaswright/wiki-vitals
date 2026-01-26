import { writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { JSDOM } from "jsdom";
import { fetchDivisions } from "../src/wiki/VitalLevel5.js";

const dom = new JSDOM("<!doctype html><html><body></body></html>");
global.DOMParser = dom.window.DOMParser;
global.Node = dom.window.Node;

const run = async () => {
  const divisions = await fetchDivisions();
  const outPath = resolve("public", "vitals-level5.json");
  const payload = {
    generatedAt: new Date().toISOString(),
    divisions,
  };
  await writeFile(outPath, JSON.stringify(payload, null, 2), "utf8");
  console.log(`Wrote ${outPath}`);
};

run().catch((err) => {
  console.error(err);
  process.exitCode = 1;
});
