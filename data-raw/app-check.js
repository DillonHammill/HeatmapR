// A check of heat_map_app() in a browser, for maintainers. Start the app with
//   Rscript -e 'shiny::runApp(HeatmapR::heat_map_app(launch.browser = FALSE), port = 7801)'
// then run: node data-raw/app-check.js http://127.0.0.1:7801/ /tmp/app (puppeteer-core, Chrome).
// Drive the HeatmapR app: wait for the heatmap, change settings, screenshot each step
const puppeteer = require("puppeteer-core");
(async () => {
  const url = process.argv[2], out = process.argv[3];
  const browser = await puppeteer.launch({ executablePath: "/usr/bin/google-chrome", headless: "new",
    args: ["--no-sandbox", "--disable-gpu"] });
  const page = await browser.newPage();
  const errors = [];
  page.on("pageerror", e => errors.push(e.message));
  page.on("console", async m => { if (m.type() === "error") { const a = await Promise.all(m.args().map(x => x.evaluate(v => v && v.stack ? v.stack : (v && v.message) || String(v)).catch(() => "?"))); errors.push(m.text() + " :: " + a.join(" | ") + " @ " + JSON.stringify(m.location())); } });
  await page.setViewport({ width: 1400, height: 950, deviceScaleFactor: 1.5 });
  await page.goto(url, { waitUntil: "networkidle0" });
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const idle = async () => {
    for (let k = 0; k < 2; k++) {
      await wait(900);
      await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 60000 });
    }
    await wait(300);
  };
  await page.waitForSelector("#plot img", { timeout: 60000 }); await idle();
  await page.screenshot({ path: out + "-1.png" });
  const report = {};
  const state = async (k) => {
    report[k] = await page.evaluate(() => ({
      img: !!document.querySelector("#plot img"),
      alert: (document.querySelector("#preview .alert") || {}).textContent || null,
      notes: document.getElementById("notes") ? document.getElementById("notes").textContent : null,
      changed: (document.getElementById("changed") || {}).textContent || ""
    }));
  };
  await state("start");
  const setSelect = async (id, value) => page.evaluate((id, value) => {
    const el = document.getElementById(id);
    if (el.selectize) el.selectize.setValue(value); else { $(el).val(value).trigger("change"); }
  }, id, value);
  const setText = async (id, value) => page.evaluate((id, value) => {
    const el = document.getElementById(id); el.value = value; $(el).trigger("change");
  }, id, value);
  // scale columns, both trees, cut rows into 3 clusters, olo
  await setSelect("hm_scale", '"column"');
  await setSelect("hm_tree", '"both"');
  await setText("hm_tree_cut_y", "3");
  await setSelect("hm_tree_order_y", '"olo"');
  await setSelect("hm_cell_col_scale", '"Blue-Red 3"');
  await setText("hm_title", "mtcars");
  await idle(); await state("settings");
  await page.screenshot({ path: out + "-2.png" });
  // search box filters settings
  await page.type("#find", "border");
  await wait(300);
  report.search = await page.evaluate(() => Array.from(document.querySelectorAll(".hm-setting"))
    .filter(d => d.style.display !== "none").map(d => d.dataset.name));
  await page.screenshot({ path: out + "-3.png" });
  await page.evaluate(() => { const f = document.getElementById("find"); f.value = ""; f.dispatchEvent(new Event("input", { bubbles: true })); });
  // R code tab
  await page.evaluate(() => { document.querySelector('a[data-value="code"]').click(); });
  await idle();
  report.code = await page.$eval("#code", e => e.textContent);
  // Explore tab
  await page.evaluate(() => { document.querySelector('a[data-value="explore"]').click(); });
  await idle(); await page.waitForSelector("#explore iframe", { timeout: 60000 }); await wait(800);
  await page.screenshot({ path: out + "-4.png" });
  // the RNA example as a dot plot
  await page.evaluate(() => { document.querySelector('a[data-value="plot"]').click(); });
  await setSelect("example", "rna"); await idle(); await wait(1500); await idle();
  await state("rna");
  await page.evaluate(() => { const r = document.querySelector('input[name="fun"][value="heat_map_dot"]'); r.click(); });
  await idle(); await wait(1000); await idle();
  await state("dot");
  await page.screenshot({ path: out + "-5.png" });
  await page.evaluate(() => { document.querySelector('a[data-value="code"]').click(); });
  await idle();
  report.dotCode = await page.$eval("#code", e => e.textContent);
  report.errors = errors;
  console.log(JSON.stringify(report, null, 1));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
