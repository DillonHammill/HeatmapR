// The screenshots of heat_map_app() in vignettes/articles/figures/app-*.png
// (and man/figures/README-app.png, the "hover" one), for maintainers. Start
// the app with
//   Rscript -e 'shiny::runApp(HeatmapR::heat_map_app(launch.browser = FALSE), port = 7801)'
// then run (puppeteer-core, Chrome):
//   node data-raw/app-screenshots.js http://127.0.0.1:7801/ /tmp/app
// and quantise the PNG files to 256 colours (e.g. with Pillow:
// Image.open(f).convert("RGB").quantize(256, dither = NONE).save(f,
// optimize = True)), which keeps them a third of the size.
const puppeteer = require("puppeteer-core");
(async () => {
  const [url, out] = process.argv.slice(2);
  const browser = await puppeteer.launch({ executablePath: "/usr/bin/google-chrome", headless: "new", args: ["--no-sandbox"] });
  const errors = [];
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const open = async (vp) => {
    const page = await browser.newPage();
    page.on("pageerror", e => errors.push("PAGE " + e.message));
    await page.setViewport(vp || { width: 1280, height: 800, deviceScaleFactor: 1.25 });
    await page.goto(url, { waitUntil: "networkidle0" });
    return page;
  };
  const idle = async (page) => { for (let k = 0; k < 4; k++) { await wait(600); await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 180000 }); } await wait(400); };
  const quiet = async (page) => page.evaluate(() => document.querySelectorAll(".shiny-notification-close").forEach(b => b.click()));
  const setFun = async (page, f) => { await page.evaluate((f) => { $('input[name="fun"][value="' + f + '"]').closest("label")[0].click(); }, f); await idle(page); };
  const pt = async (page, fx, fy) => page.evaluate((fx, fy) => { const r = document.getElementById("hm-img").getBoundingClientRect(); return { x: r.left + fx * r.width, y: r.top + fy * r.height }; }, fx, fy);
  const section = async (page, s) => { await page.click('.hm-rail-item[data-section="' + s + '"]'); await wait(500); };

  // 1. the start page
  let page = await open(); await idle(page);
  await page.screenshot({ path: out + "-start.png" });
  // 2. the workspace: mtcars, scaled by column, both dendrograms
  await page.click("#example_mtcars"); await idle(page); await wait(600);
  await section(page, "scale");
  await page.click('[data-for="hm_tree_cut_y"][data-delta="1"]'); await wait(200);
  await page.click('[data-for="hm_tree_cut_y"][data-delta="1"]');
  await idle(page); await quiet(page); await wait(400);
  await page.screenshot({ path: out + "-workspace.png" });
  // 3. hover and a selection of rows
  const mid = await pt(page, 0.42, 0.47);
  await page.mouse.move(mid.x, mid.y); await wait(300);
  await page.screenshot({ path: out + "-hover.png" });
  const a = await pt(page, 0.2, 0.30), b = await pt(page, 0.2, 0.47);
  await page.mouse.move(a.x, a.y); await page.mouse.down(); await page.mouse.move(b.x, b.y, { steps: 8 }); await page.mouse.up(); await wait(500);
  await page.screenshot({ path: out + "-select.png" });
  await page.evaluate(() => document.getElementById("hm-sel-clear").click()); await wait(300);
  // 4. colours with a histogram of the values
  await section(page, "colours");
  await page.evaluate(() => { $('input[name="hm_cell_col_scale"][value=\'"Blue-Red 3"\']').closest("label")[0].click(); });
  await page.evaluate(() => { $('input[name="hm_cell_col_scale_limits_mode"][value="custom"]').closest("label")[0].click(); });
  await wait(300);
  await page.evaluate(() => { $("#hm_cell_col_scale_limits_from").val(0).trigger("change"); $("#hm_cell_col_scale_limits_to").val(0.8).trigger("change"); });
  await idle(page);
  await page.screenshot({ path: out + "-colours.png" });
  // 5. search
  await page.type("#find", "border"); await wait(600);
  await page.screenshot({ path: out + "-search.png" });
  await page.evaluate(() => { const f = document.getElementById("find"); f.value = ""; f.dispatchEvent(new Event("input", { bubbles: true })); });
  // 6. the download dialog
  await page.click("#download_open"); await wait(1200);
  await page.screenshot({ path: out + "-download.png" });
  await page.close();

  // 7. the roles of the columns: cytometry
  page = await open(); await idle(page);
  await page.click("#example_cytometry"); await idle(page);
  await page.click("#data_chip"); await wait(600);
  await page.screenshot({ path: out + "-roles.png" });
  // 8. clusters replace the groups
  await section(page, "scale");
  await page.click('[data-for="hm_tree_cut_y"][data-delta="1"]');
  await idle(page); await wait(400);
  await page.screenshot({ path: out + "-clusters.png" });
  await page.close();

  // 9. single-cell RNA: a dot plot split by condition
  page = await open(); await idle(page);
  await page.click("#example_rna"); await idle(page);
  await page.evaluate(() => { $("#dot_split_by").val("condition").trigger("change"); }); await idle(page);
  await page.click("#data_chip"); await wait(600);
  await page.screenshot({ path: out + "-dot.png" });
  await page.close();

  // 10. a phone
  page = await open({ width: 390, height: 844, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
  await idle(page);
  await page.evaluate(() => document.getElementById("example_mtcars").click()); await idle(page); await wait(800);
  await page.screenshot({ path: out + "-phone.png" });
  await page.close();
  console.log(JSON.stringify({ errors }));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
