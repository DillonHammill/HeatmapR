// A check of heat_map_app() with single-cell data in a browser, for
// maintainers. Start the app with
//   Rscript -e 'shiny::runApp(HeatmapR::heat_map_app(launch.browser = FALSE), port = 7801)'
// then run (puppeteer-core, Chrome):
//   node data-raw/app-check-cells.js http://127.0.0.1:7801/ /tmp/cells inst/extdata/example.h5ad
// It walks the RNA and cytometry examples through the three plots, selects
// groups of a dot plot, splits grouped cells into clusters (an error, then
// "Stop grouping"), checks the settings hidden for dot plots and
// correlations, keeps a dot plot's settings through a change of plot and its
// undo, and uploads an AnnData file. It prints what the app showed and any
// errors of the page, and takes screenshots.
const puppeteer = require("puppeteer-core");
(async () => {
  const [url, out, h5ad] = process.argv.slice(2);
  const browser = await puppeteer.launch({ executablePath: "/usr/bin/google-chrome", headless: "new", args: ["--no-sandbox"] });
  const errors = [];
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const open = async () => {
    const page = await browser.newPage();
    page.on("pageerror", e => errors.push("PAGE " + e.message));
    page.on("console", m => { if (m.type() === "error" || m.type() === "warning") errors.push("CONSOLE " + m.text()); });
    await page.setViewport({ width: 1440, height: 900 });
    await page.goto(url, { waitUntil: "networkidle0" });
    return page;
  };
  const idle = async (page) => {
    for (let k = 0; k < 4; k++) {
      await wait(600);
      await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 180000 });
    }
    await wait(300);
  };
  const state = async (page) => page.evaluate(() => ({
    fun: $("input[name=fun]:checked").val(),
    issue: document.getElementById("issue").innerText.trim(),
    chips: document.getElementById("changed").innerText.replace(/\s+/g, " ").trim(),
    data: document.getElementById("data_chip").innerText.replace(/\s+/g, " ").trim(),
    status: document.getElementById("status").innerText.split("\n")[0],
    outputErrors: Array.from(document.querySelectorAll(".shiny-output-error")).map(e => e.id),
    code: document.getElementById("hm-code-text").innerText.split("\n").slice(2, -2).join(" | ")
  }));
  const setFun = async (page, f) => {
    await page.evaluate((f) => { $('input[name="fun"][value="' + f + '"]').closest("label")[0].click(); }, f);
    await idle(page);
  };
  const R = {};

  // RNA: a dot plot of cell types; two groups and two features kept
  let page = await open(); await idle(page);
  await page.click("#example_rna"); await idle(page);
  R.rna_dot = await state(page);
  await page.evaluate(() => Shiny.setInputValue("hm_selection", { rows: ["B", "NK"], cols: ["MS4A1", "CD79A", "GNLY"] }, { priority: "event" }));
  await page.evaluate(() => document.getElementById("sel_show").click()); await idle(page);
  R.rna_groups = await state(page);
  await page.screenshot({ path: out + "-rna-groups.png" });
  // Kendall correlations of the features kept, and the settings hidden
  await setFun(page, "heat_map_cor");
  await page.evaluate(() => { $("#cor_method").val('"kendall"').trigger("change"); }); await idle(page);
  R.rna_cor = await state(page);
  R.rna_cor_hidden = await page.evaluate(() => Array.from(document.querySelectorAll(".hm-unused-note")).map(n => n.innerText));
  // A dot plot's setting kept through a heatmap, and the change of plot undone
  await setFun(page, "heat_map_dot");
  await page.evaluate(() => { $("#dot_threshold").val(1).trigger("change"); }); await idle(page);
  await setFun(page, "heat_map");
  await setFun(page, "heat_map_dot");
  R.dot_kept = await page.evaluate(() => $("#dot_threshold").val());
  await page.click("#undo"); await idle(page);
  R.undone = await page.evaluate(() => ({ fun: $("input[name=fun]:checked").val(), threshold: $("#dot_threshold").val() }));
  await page.close();

  // Cytometry: grouped by cluster as a heatmap, a dot plot and correlations
  page = await open(); await idle(page);
  await page.click("#example_cytometry"); await idle(page);
  R.cyto = await state(page);
  await setFun(page, "heat_map_dot");
  R.cyto_dot = await state(page);
  await page.screenshot({ path: out + "-cyto-dot.png" });
  await setFun(page, "heat_map_cor");
  await page.evaluate(() => { $("#cor_method").val('"kendall"').trigger("change"); });
  const t0 = Date.now(); await idle(page); R.cyto_kendall_ms = Date.now() - t0;
  R.cyto_cor = await state(page);
  // Clusters of grouped cells: an error that offers to stop grouping
  await setFun(page, "heat_map");
  await page.click('.hm-rail-item[data-section="scale"]'); await wait(300);
  for (let k = 0; k < 3; k++) { await page.click('[data-for="hm_tree_cut_y"][data-delta="1"]'); await wait(150); }
  await idle(page);
  R.cyto_cut = await state(page);
  await page.screenshot({ path: out + "-cyto-issue.png" });
  await page.evaluate(() => { const b = document.querySelector('#issue [data-reset="__group"]'); if (b) b.click(); });
  await idle(page); await wait(500); await idle(page);
  R.cyto_ungrouped = await state(page);
  await page.screenshot({ path: out + "-cyto-clusters.png" });
  await page.close();

  // An AnnData file: grouped by its cell types
  if (h5ad) {
    page = await open(); await idle(page);
    const input = await page.$("#file"); await input.uploadFile(h5ad);
    await idle(page); await wait(800); await idle(page);
    R.h5ad = await state(page);
    await setFun(page, "heat_map_dot");
    R.h5ad_dot = await state(page);
    await page.close();
  }
  R.errors = errors;
  console.log(JSON.stringify(R, null, 1));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
