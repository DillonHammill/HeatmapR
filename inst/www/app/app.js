/* HeatmapR app: sections, search, steppers, colours, the heatmap canvas
   (values on hover, selection by click or drag, zoom) and the R code */
(function() {
  "use strict";
  var $doc = $(document);
  function send(name, value) { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(name, value, { priority: "event" }); }
  function el(id) { return document.getElementById(id); }

  // ---- views and sections ----
  var section = "scale";
  function showSection(id) {
    section = id;
    var panel = el("hm-panel"); if (!panel) return;
    var all = id === "all";
    document.querySelectorAll(".hm-rail-item").forEach(function(b) {
      b.setAttribute("aria-pressed", b.dataset.section === id ? "true" : "false");
    });
    document.querySelectorAll(".hm-section").forEach(function(s) {
      var on = all ? !(s.dataset.section === "fun" && funHidden()) : s.dataset.section === id;
      s.hidden = !on;
      if (all) { var more = s.querySelector("details.hm-more"); if (more) more.open = true; }
    });
    if (all) { var f = el("find"); if (f) f.focus(); }
    panel.scrollTop = 0;
    // (OUTPUTS IN A SECTION THAT WAS HIDDEN ARE DRAWN ONCE SHINY KNOWS IT SHOWS)
    $(".hm-section:not([hidden])").trigger("shown");
  }
  function funHidden() {
    var b = document.querySelector('.hm-rail-item[data-section="fun"]');
    return !b || b.hidden;
  }
  $doc.on("click", ".hm-rail-item", function() { var f = el("find"); if (f && f.value) { f.value = ""; filter(""); } showSection(this.dataset.section); });
  $doc.on("click", "#data_chip", function() { showSection("data"); });
  $doc.on("shiny:inputchanged", function(e) {
    if (e.name === "fun") {
      var b = document.querySelector('.hm-rail-item[data-section="fun"]');
      if (b) { b.hidden = e.value === "heat_map"; }
      if (e.value !== "heat_map") showSection("fun"); else if (section === "fun") showSection("scale");
    }
  });
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("hm-view", function(m) {
      var app = el("hm-app"); if (!app) return;
      app.classList.toggle("hm-on-start", m.view === "start");
      app.classList.toggle("hm-on-work", m.view !== "start");
      if (m.section) showSection(m.section);
      window.scrollTo(0, 0);
      // (OUTPUTS IN A VIEW THAT WAS HIDDEN ARE DRAWN ONCE SHINY KNOWS IT SHOWS)
      $(m.view === "start" ? "#hm-start" : "#hm-work").trigger("shown");
      $(window).trigger("resize");
      setTimeout(fit, 50);
    });
    Shiny.addCustomMessageHandler("hm-section", function(m) { showSection(m.section); });
  }

  // ---- search ----
  function filter(q) {
    q = (q || "").toLowerCase().trim();
    var panel = el("hm-panel"); if (!panel) return;
    panel.classList.toggle("hm-searching", !!q);
    var any = false;
    document.querySelectorAll(".hm-section").forEach(function(s) {
      var hits = 0;
      s.querySelectorAll(".hm-setting").forEach(function(d) {
        var hit = !d.classList.contains("hm-unused") && !d.closest(".hm-fun-block[hidden]") &&
          (!q || (d.dataset.search || "").indexOf(q) >= 0);
        d.style.display = hit ? "" : "none";
        if (hit) hits++;
        // (SETTINGS NAMED BY THE WORDS FIRST, THEN THOSE WHOSE HELP HAS THEM)
        var top = d.parentNode.classList.contains("hm-pair") ? d.parentNode : d;
        top.style.order = q && (d.dataset.label || "").indexOf(q) < 0 ? 1 : "";
      });
      s.querySelectorAll(".hm-pair").forEach(function(p) {
        var shown = Array.prototype.some.call(p.children, function(c) { return c.style.display !== "none"; });
        p.style.display = shown ? "" : "none";
      });
      // (WHAT HOLDS NO SETTING - THE DATA'S CARD AND ROLES - HIDES WHILE SEARCHING)
      Array.prototype.forEach.call(s.children, function(c) {
        if (c.classList.contains("hm-section-head")) return;
        var has = c.classList.contains("hm-setting") || c.querySelector(".hm-setting");
        if (!has) c.style.display = q ? "none" : "";
      });
      if (q) {
        s.hidden = hits === 0 || (s.dataset.section === "fun" && funHidden());
        if (!s.hidden) any = true;
        s.querySelectorAll("details.hm-more").forEach(function(m) { m.open = true; });
        // (SECTIONS WITH A SETTING NAMED BY THE WORDS FIRST)
        var named = Array.prototype.some.call(s.querySelectorAll(".hm-setting"), function(d) {
          return (d.dataset.label || "").indexOf(q) >= 0;
        });
        s.style.order = named ? "" : 1;
      } else {
        s.style.order = "";
      }
    });
    var none = document.querySelector(".hm-find-none"); if (none) none.hidden = !q || any;
    if (!q) showSection(section === "all" ? "scale" : section);
  }
  $doc.on("input", "#find", function() { filter(this.value); });
  $doc.on("keydown", "#find", function(e) { if (e.key === "Escape") { this.value = ""; filter(""); } });

  // ---- help, steppers, colours, resets ----
  $doc.on("click", ".hm-info", function() {
    var doc = this.parentNode.querySelector(".hm-doc"); if (!doc) return;
    doc.hidden = !doc.hidden; this.setAttribute("aria-expanded", doc.hidden ? "false" : "true");
  });
  $doc.on("click", ".hm-step", function() {
    var input = el(this.dataset.for); if (!input) return;
    var v = parseFloat(input.value); if (isNaN(v)) v = 1;
    v = Math.max(0, v + parseFloat(this.dataset.delta));
    if (v === 1) v = parseFloat(this.dataset.delta) > 0 ? 2 : 0;
    input.value = v < 2 ? "" : String(v);
    $(input).trigger("change");
  });
  $doc.on("click", "[data-reset]", function() { send("reset_one", this.dataset.reset); });
  $doc.on("click", ".hm-reset-section", function() { send("reset_section", this.dataset.section); });
  function hex(c) {
    var x = document.createElement("canvas").getContext("2d");
    x.fillStyle = "#000000"; x.fillStyle = c; var a = x.fillStyle;
    x.fillStyle = "#ffffff"; x.fillStyle = c; var b = x.fillStyle;
    return a === b && /^#[0-9a-f]{6}$/i.test(a) ? a : null;
  }
  function sync(t) {
    var s = t.closest(".hm-colour") && t.closest(".hm-colour").querySelector(".hm-swatch");
    if (s) { var v = hex(t.value); s.value = v || "#ffffff"; s.style.opacity = v ? 1 : 0.4; }
  }
  $doc.on("input", ".hm-swatch", function() {
    var t = el(this.dataset.for); if (t) { t.value = this.value; $(t).trigger("change"); }
  });
  $doc.on("shiny:inputchanged", function(e) { var t = el(e.name); if (t && t.closest && t.closest(".hm-colour")) sync(t); });
  $doc.on("shiny:connected", function() { document.querySelectorAll(".hm-colour input[type=text]").forEach(sync); });
  $doc.on("change input", ".hm-colour input[type=text]", function() { sync(this); });

  // ---- drag a file onto the start page ----
  $doc.on("dragenter dragover", ".hm-drop", function(e) { e.preventDefault(); this.classList.add("hm-dragging"); });
  $doc.on("dragleave", ".hm-drop", function() { this.classList.remove("hm-dragging"); });
  $doc.on("drop", ".hm-drop", function(e) {
    e.preventDefault(); this.classList.remove("hm-dragging");
    var files = e.originalEvent.dataTransfer && e.originalEvent.dataTransfer.files;
    var input = this.querySelector("input[type=file]");
    if (files && files.length && input) {
      try { input.files = files; } catch (err) { return; }
      $(input).trigger("change");
    }
  });

  // ---- undo, redo and the chips' counts ----
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("hm-history", function(m) {
      var u = el("undo"), r = el("redo");
      if (u) u.disabled = !m.undo; if (r) r.disabled = !m.redo;
    });
    // (SETTINGS OF heat_map() THAT THE DOT PLOT OR CORRELATIONS DON'T USE)
    Shiny.addCustomMessageHandler("hm-unused", function(m) {
      var names = m.names || [];
      var what = m.fun === "heat_map_dot" ? "dot plots" : "correlations";
      document.querySelectorAll(".hm-fun-block").forEach(function(b) { b.hidden = b.dataset.fun !== m.fun; });
      document.querySelectorAll(".hm-setting[data-id]").forEach(function(d) {
        d.classList.toggle("hm-unused", d.dataset.id === "hm_" + d.dataset.name &&
          names.indexOf(d.dataset.name) >= 0);
      });
      document.querySelectorAll(".hm-pair").forEach(function(p) {
        var all = p.querySelectorAll(".hm-setting").length > 0 &&
          Array.prototype.every.call(p.querySelectorAll(".hm-setting"), function(d) { return d.classList.contains("hm-unused"); });
        p.classList.toggle("hm-unused", all);
      });
      document.querySelectorAll(".hm-section").forEach(function(s) {
        var n = s.querySelectorAll(".hm-setting.hm-unused").length;
        var note = s.querySelector(".hm-unused-note");
        if (!n) { if (note) note.remove(); return; }
        if (!note) {
          note = document.createElement("p"); note.className = "hm-hint hm-unused-note";
          var head = s.querySelector(".hm-section-head");
          if (head && head.nextSibling) s.insertBefore(note, head.nextSibling); else s.appendChild(note);
        }
        note.textContent = (n === 1 ? "One setting" : n + " settings") + " of heatmaps " +
          (n === 1 ? "isn't" : "aren't") + " used by " + what + " and " + (n === 1 ? "is" : "are") + " hidden.";
      });
      var f = el("find"); if (f && f.value) filter(f.value);
    });
    Shiny.addCustomMessageHandler("hm-badges", function(m) {
      document.querySelectorAll(".hm-badge").forEach(function(b) {
        var n = m[b.dataset.section] || 0; b.textContent = n ? String(n) : "";
      });
    });
  }

  // ---- the R code ----
  function esc(s) { return String(s).replace(/[&<>"']/g, function(c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]; }); }
  function highlight(line) {
    var out = "", re = /("(?:[^"\\]|\\.)*")|(\b[A-Za-z_.][A-Za-z0-9_.]*(?=\())|(\b\d+(?:\.\d+)?(?:e[+-]?\d+)?\b)/g, last = 0, m;
    while ((m = re.exec(line))) {
      out += esc(line.slice(last, m.index));
      out += '<span class="' + (m[1] ? "s" : m[2] ? "f" : "n") + '">' + esc(m[0]) + "</span>";
      last = re.lastIndex;
    }
    return out + esc(line.slice(last));
  }
  var codeText = "";
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("hm-code", function(m) {
      codeText = m.code || "";
      var pre = el("hm-code-text"); if (!pre) return;
      var lines = codeText.split("\n"), html = "";
      lines.forEach(function(l) {
        var now = m.changed && new RegExp("^\\s*" + m.changed.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + " = ").test(l);
        html += '<span class="l' + (now ? " now" : "") + '">' + (highlight(l) || " ") + "</span>";
      });
      pre.innerHTML = html;
    });
  }
  function copyText(text, button) {
    function done() { if (!button) return; var o = button.textContent; button.textContent = "Copied"; setTimeout(function() { button.textContent = o; }, 1200); }
    if (navigator.clipboard && window.isSecureContext) { navigator.clipboard.writeText(text).then(done, fallback); } else fallback();
    function fallback() {
      var ta = document.createElement("textarea"); ta.value = text; ta.style.position = "fixed"; ta.style.opacity = "0";
      document.body.appendChild(ta); ta.select(); try { document.execCommand("copy"); done(); } catch (err) {} document.body.removeChild(ta);
    }
  }
  $doc.on("click", "#hm-code-copy", function() { copyText(codeText, this); });
  $doc.on("click", "#code_toggle", function() {
    var app = el("hm-app"), closed = app.classList.toggle("hm-code-closed");
    this.setAttribute("aria-pressed", closed ? "false" : "true");
    setTimeout(fit, 30);
  });

  // ---- the heatmap: values on hover, selection, zoom ----
  var D = null, zoom = 1, baseW = 672, baseH = 672, selR = new Set(), selC = new Set(), drag = null, press = null;
  function find(starts, ends, v) {
    var lo = 0, hi = starts.length - 1;
    while (lo <= hi) { var mid = (lo + hi) >> 1; if (v < starts[mid]) hi = mid - 1; else if (v >= ends[mid]) lo = mid + 1; else return mid; }
    return -1;
  }
  function frac(e) { var r = el("hm-img").getBoundingClientRect(); return { x: (e.clientX - r.left) / r.width, y: (e.clientY - r.top) / r.height }; }
  function inRows(p) { return D && p.y >= D.box.top && p.y < D.box.bottom; }
  function inCols(p) { return D && p.x >= D.box.left && p.x < D.box.right; }
  function num(v) {
    if (v === null || v === undefined) return "NA";
    if (typeof v !== "number") return String(v);
    if (v === 0) return "0";
    var a = Math.abs(v);
    if (a >= 1e5 || a < 1e-3) return v.toExponential(3).replace(/\.?0+e/, "e");
    return String(Number(v.toPrecision(4)));
  }
  function info(part, i) {
    var out = [];
    for (var k in part.info) if (Object.prototype.hasOwnProperty.call(part.info, k)) {
      var v = part.info[k][i]; if (v !== null && v !== undefined && v !== "") out.push('<span class="k">' + esc(k) + ":</span> " + esc(num(v)));
    }
    return out.join(" &middot; ");
  }
  function tipHtml(i, j) {
    var R = D.rows, C = D.cols, h = "<b>" + esc(R.label[i]) + "</b> &times; <b>" + esc(C.label[j]) + "</b>";
    var v = D.value ? D.value[i][j] : null, raw = D.raw ? D.raw[i][j] : null;
    if (D.raw && D.value && v !== null) h += "<br>" + esc(D.rawLabel) + " " + esc(num(raw)) + ' <span class="k">(' + esc(D.valueLabel) + " " + esc(num(v)) + ")</span>";
    else if (D.raw) h += "<br>" + esc(num(raw));
    else if (D.value) h += "<br>" + esc(D.valueLabel) + " " + esc(num(v));
    if (D.size) h += "<br>" + esc(D.sizeLabel) + " " + esc(num(D.size[i][j]));
    var ri = info(R, i), ci = info(C, j);
    if (ri) h += "<br>" + ri; if (ci) h += "<br>" + ci;
    return h;
  }
  function showTip(html, e) {
    var tip = el("hm-tip"); tip.innerHTML = html; tip.hidden = false;
    var w = tip.offsetWidth, h = tip.offsetHeight, x = e.clientX + 14, y = e.clientY + 14;
    if (x + w > window.innerWidth - 4) x = e.clientX - w - 14;
    if (y + h > window.innerHeight - 4) y = e.clientY - h - 14;
    tip.style.left = Math.max(4, x) + "px"; tip.style.top = Math.max(4, y) + "px";
  }
  function hideTip() { var t = el("hm-tip"); if (t) t.hidden = true; }
  function rect(cls, l, t, r, b) {
    var d = document.createElement("div"); d.className = cls;
    d.style.left = (100 * l) + "%"; d.style.top = (100 * t) + "%";
    d.style.width = (100 * (r - l)) + "%"; d.style.height = (100 * (b - t)) + "%";
    return d;
  }
  var layers = {};
  function layer(name) {
    if (!layers[name]) { var d = document.createElement("div"); d.style.inset = "0"; layers[name] = d; el("hm-overlay").appendChild(d); }
    return layers[name];
  }
  function runs(set, n) {
    var out = [], start = -1;
    for (var i = 0; i <= n; i++) { var on = i < n && set.has(i); if (on && start < 0) start = i; if (!on && start >= 0) { out.push([start, i - 1]); start = -1; } }
    return out;
  }
  function drawSelection() {
    if (!D) return;
    var L = layer("sel"); L.innerHTML = "";
    var R = D.rows, C = D.cols, box = D.box;
    runs(selR, R.label.length).forEach(function(r) {
      L.appendChild(rect("sel-band", 0, R.top[r[0]], box.left, R.bottom[r[1]]));
      L.appendChild(rect("sel", box.left, R.top[r[0]], box.right, R.bottom[r[1]]));
    });
    runs(selC, C.label.length).forEach(function(r) {
      L.appendChild(rect("sel-band", C.left[r[0]], box.bottom, C.right[r[1]], 1));
      L.appendChild(rect("sel", C.left[r[0]], box.top, C.right[r[1]], box.bottom));
    });
    var bar = el("hm-selbar"), n = selR.size, m = selC.size;
    if (bar) {
      bar.hidden = !(n || m);
      var parts = []; if (n) parts.push(n + (n === 1 ? " row" : " rows")); if (m) parts.push(m + (m === 1 ? " column" : " columns"));
      el("hm-selcount").textContent = parts.join(" and ") + " selected";
    }
    var sorted = function(s) { return Array.from(s).sort(function(a, b) { return a - b; }); };
    send("hm_selection", { rows: sorted(selR).map(function(i) { return R.label[i]; }), cols: sorted(selC).map(function(j) { return C.label[j]; }) });
  }
  function drawHover(i, j) {
    var L = layer("hover"); L.innerHTML = ""; if (!D) return;
    if (i >= 0) L.appendChild(rect("hover", D.box.left, D.rows.top[i], D.box.right, D.rows.bottom[i]));
    if (j >= 0) L.appendChild(rect("hover", D.cols.left[j], D.box.top, D.cols.right[j], D.box.bottom));
  }
  function hoverAt(e) {
    if (!D) return;
    var p = frac(e), i = inRows(p) ? find(D.rows.top, D.rows.bottom, p.y) : -1, j = inCols(p) ? find(D.cols.left, D.cols.right, p.x) : -1;
    var on = !el("hm-hover") || el("hm-hover").checked;
    drawHover(i, j);
    if (!on) { hideTip(); return; }
    if (i >= 0 && j >= 0) showTip(tipHtml(i, j), e);
    else if (i >= 0) { var ri = info(D.rows, i); showTip("<b>" + esc(D.rows.label[i]) + "</b>" + (ri ? "<br>" + ri : ""), e); }
    else if (j >= 0) { var ci = info(D.cols, j); showTip("<b>" + esc(D.cols.label[j]) + "</b>" + (ci ? "<br>" + ci : ""), e); }
    else hideTip();
  }
  function toggle(set, i) { if (set.has(i)) set.delete(i); else set.add(i); }
  function endDrag(d) {
    var s = d.start;
    if (!d.moved) {
      var i = inRows(s) ? find(D.rows.top, D.rows.bottom, s.y) : -1, j = inCols(s) ? find(D.cols.left, D.cols.right, s.x) : -1;
      if (i >= 0) toggle(selR, i); else if (j >= 0) toggle(selC, j);
      drawSelection(); return;
    }
    var l = Math.min(s.x, d.end.x), r = Math.max(s.x, d.end.x), t = Math.min(s.y, d.end.y), b = Math.max(s.y, d.end.y), box = D.box;
    var acrossCols = l < box.right && r > box.left, acrossRows = t < box.bottom && b > box.top;
    if (acrossRows) for (var i2 = 0; i2 < D.rows.label.length; i2++) if (D.rows.bottom[i2] > t && D.rows.top[i2] < b) d.remove ? selR.delete(i2) : selR.add(i2);
    if (acrossCols) for (var j2 = 0; j2 < D.cols.label.length; j2++) if (D.cols.right[j2] > l && D.cols.left[j2] < r) d.remove ? selC.delete(j2) : selC.add(j2);
    drawSelection();
  }
  function moveDrag(e) {
    var p = frac(e), b = layer("brush");
    drag.end = p; drag.moved = drag.moved || Math.abs(e.clientX - drag.cx) + Math.abs(e.clientY - drag.cy) > 4;
    b.innerHTML = ""; if (drag.moved) b.appendChild(rect("brush", Math.min(drag.start.x, p.x), Math.min(drag.start.y, p.y), Math.max(drag.start.x, p.x), Math.max(drag.start.y, p.y)));
    hideTip();
  }
  function bindStage() {
    var stage = el("hm-stage"); if (!stage || stage.dataset.bound) return; stage.dataset.bound = "1";
    stage.addEventListener("pointermove", function(e) {
      if (!D) return;
      if (press && !drag) { if (Math.abs(e.clientX - press.x) + Math.abs(e.clientY - press.y) > 8) { clearTimeout(press.timer); press = null; } return; }
      if (drag) { moveDrag(e); return; }
      if (e.pointerType !== "touch") hoverAt(e);
    });
    stage.addEventListener("pointerleave", function(e) { if (!drag && e.pointerType !== "touch") { hideTip(); drawHover(-1, -1); } });
    stage.addEventListener("pointerdown", function(e) {
      if (!D || !e.isPrimary) return;
      if (e.pointerType === "touch") {
        var start = { clientX: e.clientX, clientY: e.clientY };
        press = { x: e.clientX, y: e.clientY, timer: setTimeout(function() { press = null; drag = { start: frac(start), end: frac(start), cx: start.clientX, cy: start.clientY, moved: false }; }, 400) };
        return;
      }
      if (e.button !== 0) return;
      e.preventDefault();
      drag = { start: frac(e), end: frac(e), cx: e.clientX, cy: e.clientY, moved: false, remove: e.altKey };
      try { stage.setPointerCapture(e.pointerId); } catch (err) {}
    });
    stage.addEventListener("touchmove", function(e) { if (drag) e.preventDefault(); }, { passive: false });
    window.addEventListener("pointerup", function(e) {
      if (press) { clearTimeout(press.timer); press = null; hoverAt(e); drag = { start: frac(e), end: frac(e), cx: e.clientX, cy: e.clientY, moved: false }; }
      if (!drag) return;
      var d = drag; drag = null; layer("brush").innerHTML = ""; endDrag(d);
    });
  }
  function fitZoom() {
    var c = el("hm-canvas"); if (!c) return 1;
    var w = c.clientWidth - 34, h = Math.max(window.innerHeight - c.getBoundingClientRect().top - 90, 300);
    return Math.max(0.1, Math.min(1.5, w / baseW, h / baseH));
  }
  function setZoom(z) { zoom = Math.max(0.1, Math.min(8, z)); var s = el("hm-stage"); if (s) s.style.width = (baseW * zoom) + "px"; }
  function fit() { if (D) setZoom(fitZoom()); }
  $doc.on("click", "#hm-zoom-in", function() { setZoom(zoom * 1.25); });
  $doc.on("click", "#hm-zoom-out", function() { setZoom(zoom / 1.25); });
  $doc.on("click", "#hm-zoom-fit", fit);
  $(window).on("resize", function() { clearTimeout(window.hmFit); window.hmFit = setTimeout(fit, 120); });
  $doc.on("click", "#hm-sel-clear", function() { selR.clear(); selC.clear(); drawSelection(); });
  $doc.on("click", "#hm-sel-copy", function() {
    if (!D) return;
    var names = Array.from(selR).sort(function(a, b) { return a - b; }).map(function(i) { return D.rows.label[i]; })
      .concat(Array.from(selC).sort(function(a, b) { return a - b; }).map(function(j) { return D.cols.label[j]; }));
    copyText(names.join("\n"), this);
  });
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("hm-draw", function(m) {
      var img = el("hm-img"), first = !D;
      var data = JSON.parse(m.data);
      // (A SELECTION OUTLIVES A REDRAW WHEN THE ROWS AND COLUMNS ARE THE SAME)
      var same = D && D.rows.label.join("\u0001") === data.rows.label.join("\u0001") && D.cols.label.join("\u0001") === data.cols.label.join("\u0001");
      if (!same) { selR.clear(); selC.clear(); }
      D = data; baseW = m.width; baseH = m.height;
      img.onload = function() { var e = el("hm-empty"); if (e) e.hidden = true; if (first || !same) fit(); drawSelection(); };
      img.src = m.image;
      bindStage();
    });
    Shiny.addCustomMessageHandler("hm-sel-clear", function(m) { selR.clear(); selC.clear(); drawSelection(); });
  }
})();
