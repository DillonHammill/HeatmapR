// Run with: node data-raw/html-touch-check.js page.html shot.png (puppeteer-core, Chrome)
// Touch on a phone-sized screen: a tap shows values and selects a row; a quick swipe scrolls
// and selects nothing; press and hold then drag selects a box of rows.
const puppeteer = require("puppeteer-core");
(async () => {
  const [file, shot] = process.argv.slice(2);
  const browser = await puppeteer.launch({ executablePath: "/usr/bin/google-chrome", headless: "new",
    args: ["--no-sandbox", "--disable-gpu"] });
  const page = await browser.newPage();
  const errors = [];
  page.on("pageerror", e => errors.push(e.message));
  await page.setViewport({ width: 390, height: 844, deviceScaleFactor: 3, isMobile: true, hasTouch: true });
  await page.goto("file://" + file);
  await page.evaluate(() => new Promise(r => { const i = document.getElementById("image"); if (i.complete) r(); else i.onload = r; }));
  const D = await page.evaluate(() => window.HeatmapR.data);
  const pt = (fx, fy) => page.evaluate((fx, fy) => {
    const r = document.getElementById("image").getBoundingClientRect();
    return { x: r.left + fx * r.width, y: r.top + fy * r.height };
  }, fx, fy);
  const sel = () => page.evaluate(() => window.HeatmapR.selection());
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const cellX = (D.cols.left[2] + D.cols.right[2]) / 2, rowY = i => (D.rows.top[i] + D.rows.bottom[i]) / 2;
  // tap
  const a = await pt(cellX, rowY(1));
  await page.touchscreen.tap(a.x, a.y); await wait(100);
  const tap = { sel: await sel(), tip: await page.$eval("#tip", t => t.style.display) };
  // quick swipe (no hold): no new selection
  await page.evaluate(() => document.getElementById("clear").click());
  const s0 = await pt(cellX, rowY(3)), s1 = await pt(cellX, rowY(8));
  await page.touchscreen.touchStart(s0.x, s0.y);
  for (let k = 1; k <= 6; k++) { await page.touchscreen.touchMove(s0.x, s0.y + (s1.y - s0.y) * k / 6); await wait(16); }
  await page.touchscreen.touchEnd(); await wait(100);
  const swipe = await sel();
  // hold, then drag over the row labels from row 3 to row 6
  const h0 = await pt(D.box.left * 0.5, rowY(3)), h1 = await pt(D.box.left * 0.4, rowY(6));
  await page.touchscreen.touchStart(h0.x, h0.y); await wait(550);
  for (let k = 1; k <= 6; k++) { await page.touchscreen.touchMove(h0.x, h0.y + (h1.y - h0.y) * k / 6); await wait(16); }
  await page.touchscreen.touchEnd(); await wait(100);
  const hold = await sel();
  await page.screenshot({ path: shot, fullPage: true });
  const layout = await page.evaluate(() => {
    const v = document.getElementById("viewport").getBoundingClientRect(), c = document.getElementById("canvas").getBoundingClientRect();
    return { viewportHeight: v.height, canvasHeight: c.height };
  });
  console.log(JSON.stringify({ tap, swipe, hold, expectedHold: D.rows.label.slice(3, 7), layout, errors }, null, 1));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
