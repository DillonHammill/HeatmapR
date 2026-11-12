// A check of the interactive page of heat_map_html() in a browser, for
// maintainers: hover over a cell, click it, drag across the row labels and the
// column dendrogram, search, and take a screenshot. Needs Chrome and
// puppeteer-core (npm install puppeteer-core in a scratch folder):
//
//   node html-check.js /path/to/page.html /path/to/screenshot.png "search text"
//
// It prints the tooltip, the selections and any errors of the page as JSON.
// Drive an interactive heatmap page: hover a cell, click, drag, search, and screenshot
const puppeteer = require("puppeteer-core");
(async () => {
  const file = process.argv[2], shot = process.argv[3];
  const browser = await puppeteer.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: "new",
    args: ["--no-sandbox", "--disable-gpu"] });
  const page = await browser.newPage();
  const errors = [];
  page.on("pageerror", e => errors.push(e.message));
  page.on("console", m => { if (m.type() === "error") errors.push(m.text()); });
  await page.setViewport({ width: 1100, height: 800 });
  await page.goto("file://" + file);
  await page.waitForSelector("#image");
  await page.evaluate(() => new Promise(r => { const i = document.getElementById("image"); if (i.complete) r(); else i.onload = r; }));
  const D = await page.evaluate(() => window.HeatmapR.data);
  const pt = async (fx, fy) => page.evaluate((fx, fy) => {
    const r = document.getElementById("image").getBoundingClientRect();
    return { x: r.left + fx * r.width, y: r.top + fy * r.height };
  }, fx, fy);
  // hover the cell of row 3, column 2
  const i = 2, j = 1;
  const c = await pt((D.cols.left[j] + D.cols.right[j]) / 2, (D.rows.top[i] + D.rows.bottom[i]) / 2);
  await page.mouse.move(c.x, c.y);
  const tip = await page.$eval("#tip", t => ({ shown: t.style.display, text: t.textContent }));
  // click: select row 3
  await page.mouse.down(); await page.mouse.up();
  let sel = await page.evaluate(() => window.HeatmapR.selection());
  // drag across rows 5 to 8 over the row labels (left of the cells)
  const n = D.rows.label.length, r0 = Math.min(4, n - 2), r1 = Math.min(7, n - 1);
  const a = await pt(D.box.left / 2, (D.rows.top[r0] + D.rows.bottom[r0]) / 2);
  const b = await pt(D.box.left / 3, (D.rows.top[r1] + D.rows.bottom[r1]) / 2);
  await page.mouse.move(a.x, a.y); await page.mouse.down(); await page.mouse.move(b.x, b.y, { steps: 5 }); await page.mouse.up();
  const sel2 = await page.evaluate(() => window.HeatmapR.selection());
  // search
  await page.type("#search", process.argv[4] || "a");
  const found = await page.$eval("#found", f => f.textContent);
  await page.keyboard.press("Enter");
  const sel3 = await page.evaluate(() => window.HeatmapR.selection());
  await page.mouse.move(c.x, c.y);
  // drag across columns over the column dendrogram (above the cells)
  await page.click("#clear");
  const ca = await pt(D.cols.left[1] + 0.001, D.box.top / 2), cb = await pt(D.cols.right[3] - 0.001, D.box.top / 3);
  await page.mouse.move(ca.x, ca.y); await page.mouse.down(); await page.mouse.move(cb.x, cb.y, { steps: 5 }); await page.mouse.up();
  const colDrag = await page.evaluate(() => window.HeatmapR.selection());
  await page.keyboard.press("Enter");
  await page.mouse.move(c.x, c.y);
  await page.screenshot({ path: shot });
  console.log(JSON.stringify({ tip, expected: [D.rows.label[i], D.cols.label[j]], afterClick: sel,
    afterDrag: sel2.rows, expectedDrag: D.rows.label.slice(r0, r1 + 1), found, afterSearch: sel3.rows.length + " rows / " + sel3.cols.length + " cols", colDrag, expectedCols: D.cols.label.slice(1, 4), errors }, null, 1));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
