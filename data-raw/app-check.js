// A check of heat_map_app() in a browser, for maintainers. Start the app with
//   Rscript -e 'shiny::runApp(HeatmapR::heat_map_app(launch.browser = FALSE), port = 7801)'
// then run: node data-raw/app-check.js http://127.0.0.1:7801/ /tmp/app (puppeteer-core, Chrome).
// Walk through the redesigned app in Chrome and take screenshots of each part
const puppeteer = require("puppeteer-core");
(async () => {
  const [url, out] = process.argv.slice(2);
  const browser = await puppeteer.launch({ executablePath: "/usr/bin/google-chrome", headless: "new", args: ["--no-sandbox"] });
  const errors = [];
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const open = async (vp) => {
    const page = await browser.newPage();
    page.on("pageerror", e => errors.push("PAGE " + e.message));
    page.on("console", m => { if (m.type() === "error") errors.push("CONSOLE " + m.text()); });
    await page.setViewport(vp);
    await page.goto(url, { waitUntil: "networkidle0" });
    return page;
  };
  const idle = async (page) => { for (let k = 0; k < 3; k++) { await wait(700); await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 90000 }); } await wait(300); };
  const R = {};
  // 1. the RNA example as a dot plot
  let page = await open({ width: 1440, height: 900 });
  await idle(page);
  await page.click("#example_rna"); await idle(page); await wait(1500); await idle(page);
  await page.screenshot({ path: out + "-1-rna.png" });
  R.rna = await page.evaluate(() => ({ fun: $("input[name=fun]:checked").val(), code: document.getElementById("hm-code-text").innerText, issue: document.getElementById("issue").innerText }));
  await page.close();

  // 2. mtcars: hover, select, show only these, undo
  page = await open({ width: 1440, height: 900 });
  await idle(page);
  await page.click("#example_mtcars"); await idle(page); await wait(800);
  const D = await page.evaluate(() => { const r = document.getElementById("hm-img").getBoundingClientRect(); return { r: { left: r.left, top: r.top, width: r.width, height: r.height } }; });
  const geom = await page.evaluate(() => window.__hmD || null);
  // pick the centre of the cell of row 3, column 2 from the overlay geometry exposed in the payload
  const pt = async (fx, fy) => page.evaluate((fx, fy) => { const r = document.getElementById("hm-img").getBoundingClientRect(); return { x: r.left + fx * r.width, y: r.top + fy * r.height }; }, fx, fy);
  // (the payload is not global; read the box from a hover over the middle)
  const mid = await pt(0.5, 0.5);
  await page.mouse.move(mid.x, mid.y); await wait(200);
  R.tip = await page.evaluate(() => { const t = document.getElementById("hm-tip"); return { shown: !t.hidden, text: t.innerText }; });
  await page.screenshot({ path: out + "-2-hover.png" });
  // drag down the left of the cells over a band of rows
  const a = await pt(0.2, 0.40), b = await pt(0.2, 0.52);
  await page.mouse.move(a.x, a.y); await page.mouse.down(); await page.mouse.move(b.x, b.y, { steps: 8 }); await page.mouse.up(); await wait(400);
  R.sel = await page.evaluate(() => ({ bar: !document.getElementById("hm-selbar").hidden, count: document.getElementById("hm-selcount").innerText }));
  await page.screenshot({ path: out + "-3-select.png" });
  await page.click("#sel_show"); await idle(page); await wait(800);
  R.show = await page.evaluate(() => ({ chips: document.getElementById("changed").innerText, code: document.getElementById("hm-code-text").innerText }));
  await page.screenshot({ path: out + "-4-shown.png" });
  // change the scale to None, then undo
  await page.evaluate(() => { $('input[name="hm_scale"][value="FALSE"]').closest("label")[0].click(); });
  await idle(page);
  R.beforeUndo = await page.evaluate(() => ({ scale: $('input[name="hm_scale"]:checked').val(), undo: !document.getElementById("undo").disabled }));
  await page.click("#undo"); await idle(page);
  R.afterUndo = await page.evaluate(() => ({ scale: $('input[name="hm_scale"]:checked').val(), redo: !document.getElementById("redo").disabled }));
  // colours: Blue-Red 3, custom limits
  await page.click('.hm-rail-item[data-section="colours"]'); await wait(300);
  await page.evaluate(() => { $('input[name="hm_cell_col_scale"][value=\'"Blue-Red 3"\']').closest("label")[0].click(); });
  await page.evaluate(() => { $('input[name="hm_cell_col_scale_limits_mode"][value="custom"]').closest("label")[0].click(); });
  await wait(300);
  await page.evaluate(() => { $("#hm_cell_col_scale_limits_from").val(0).trigger("change"); $("#hm_cell_col_scale_limits_to").val(0.8).trigger("change"); });
  await idle(page); await wait(600);
  await page.screenshot({ path: out + "-5-colours.png" });
  R.colours = await page.evaluate(() => ({ chips: document.getElementById("changed").innerText, hist: document.querySelectorAll(".hm-hist-bars span").length, out: document.querySelectorAll(".hm-hist-bars span.out").length }));
  // search
  await page.type("#find", "border"); await wait(400);
  R.search = await page.evaluate(() => Array.from(document.querySelectorAll(".hm-setting")).filter(d => d.offsetParent !== null).map(d => d.dataset.name));
  await page.screenshot({ path: out + "-6-search.png" });
  await page.evaluate(() => { const f = document.getElementById("find"); f.value = ""; f.dispatchEvent(new Event("input", { bubbles: true })); });
  // the data section with roles (iris)
  await page.select("#example", "iris"); await idle(page); await wait(800);
  await page.click("#data_chip"); await wait(400);
  await page.screenshot({ path: out + "-7-data.png" });
  R.iris = await page.evaluate(() => ({ chips: document.getElementById("changed").innerText, code: document.getElementById("hm-code-text").innerText, roles: document.querySelectorAll(".hm-role").length }));
  // an error: rows split into 3 clusters and grouped by species
  await page.click('.hm-rail-item[data-section="scale"]'); await wait(200);
  await page.evaluate(() => { $("#hm_tree_cut_y").val(3).trigger("change"); });
  await idle(page);
  await page.click("#data_chip"); await wait(300);
  await page.evaluate(() => { const s = document.querySelectorAll(".hm-role select"); const sp = s[s.length - 1]; $(sp).val("group").trigger("change"); });
  await idle(page); await wait(600);
  R.issue = await page.evaluate(() => document.getElementById("issue").innerText);
  await page.screenshot({ path: out + "-8-issue.png" });
  // download dialog
  await page.click("#download_open"); await wait(1200);
  await page.screenshot({ path: out + "-9-download.png" });
  R.modal = await page.evaluate(() => !!document.querySelector(".modal-dialog"));
  await page.close();

  // 3. a phone
  page = await open({ width: 390, height: 844, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
  await idle(page);
  await page.screenshot({ path: out + "-10-phone-start.png" });
  await page.evaluate(() => document.getElementById("example_mtcars").click()); await idle(page); await wait(1000);
  await page.screenshot({ path: out + "-11-phone.png", fullPage: false });
  R.errors = errors;
  console.log(JSON.stringify(R, null, 1));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
