// Handles this page's charts, map, formatting, and scenario arithmetic. d3, Plot, and htl come from
// Observable's standard library and are passed in through index.qmd. This file contains no import
// statements because R/check.R evaluates this file in V8 to test runScenario() against R.

// Colour palette designed by Okabe and Ito to accommodate colour vision diversity. Species colours
// are consistent across all figures.
export const OKABE_ITO = {
  orange: "#E69F00", sky: "#56B4E9", green: "#009E73", yellow: "#F0E442",
  blue: "#0072B2", vermillion: "#D55E00", purple: "#CC79A7", black: "#000000"
};
export const SPECIES = ["MALL", "NOPI", "BWTE", "GADW", "NSHO", "CANV"];
export const DABBLERS = ["MALL", "NOPI", "BWTE", "GADW", "NSHO"];
export const NAMES = {
  MALL: "Mallard", NOPI: "Northern Pintail", BWTE: "Blue-winged Teal",
  GADW: "Gadwall", NSHO: "Northern Shoveler", CANV: "Canvasback"
};
export const PLURALS = {
  MALL: "Mallards", NOPI: "Northern Pintails", BWTE: "Blue-winged Teal",
  GADW: "Gadwalls", NSHO: "Northern Shovelers", CANV: "Canvasbacks"
};
export const COLORS = {
  MALL: OKABE_ITO.green, NOPI: OKABE_ITO.vermillion, BWTE: OKABE_ITO.sky,
  GADW: OKABE_ITO.orange, NSHO: OKABE_ITO.purple, CANV: OKABE_ITO.black
};
export const PONDS = OKABE_ITO.blue, DRY = OKABE_ITO.yellow;
export const INK = "#1d2125", INK2 = "#4d545b", MUTED = "#6c737a", GRID = "#e7e5df", PAPER = "#fdfcf9";
export const PROVINCES = { AB: "Alberta", SK: "Saskatchewan", MB: "Manitoba" };

// Number formats, created when first needed, using a true minus sign.
function numberFormat(opts) {
  let f = null;
  return (x) => {
    if (!f) f = new Intl.NumberFormat("en-CA", opts);
    return f.format(x).replace("-", "−");
  };
}
const int = numberFormat({ maximumFractionDigits: 0 });
const dec1 = numberFormat({ minimumFractionDigits: 1, maximumFractionDigits: 1 });
const dec2 = numberFormat({ minimumFractionDigits: 2, maximumFractionDigits: 2 });
const pct0 = numberFormat({ style: "percent", maximumFractionDigits: 0 });
export const fmt = {
  int, dec1, dec2,
  pct: pct0,
  millions: (x, d = 1) => `${numberFormat({ minimumFractionDigits: d, maximumFractionDigits: d })(x / 1e6)} million`,
  // Keep 3 significant digits: 660,378 becomes 660,000.
  approx: (x) => {
    if (!isFinite(x) || x === 0) return "0";
    const p = Math.pow(10, Math.max(0, Math.floor(Math.log10(Math.abs(x))) - 2));
    return int(Math.round(x / p) * p);
  },
  signed: (s, x) => (x < 0 ? "−" : x > 0 ? "+" : "") + s,
  // Show proportions, using "less than 1%" instead of a rounded 0%.
  share: (x) => (x > 0 && x < 0.005 ? "less than 1%" : pct0(x))
};
// Number of additional birds settling with a 10% increase in pond numbers when elasticity is e.
export const up10 = (e) => Math.pow(1.1, e) - 1;

export function kit({ d3, Plot, htl }) {
  const STYLE = { fontFamily: "var(--sans)", fontSize: "12px", color: MUTED, background: "transparent", overflow: "visible" };
  const TIP = { fill: "#fff", stroke: "#d6d3ca", fontSize: 12, textPadding: 8, lineHeight: 1.35 };
  const X_YEARS = [1960, 1970, 1980, 1990, 2000, 2010, 2020];

  // Plot labels axis and mark groups by adding aria-label to ordinary <g> elements, which screen
  // readers disregard and accessibility checkers flag. Remove these labels.
  function plot(options) {
    const el = Plot.plot({ style: STYLE, color: { type: "identity" }, ...options });
    for (const g of el.querySelectorAll("g[aria-label]")) g.removeAttribute("aria-label");
    return el;
  }

  // Redraw the chart whenever the container width changes.
  function responsive(render, minWidth = 240) {
    const div = document.createElement("div");
    div.className = "chart";
    let last = 0;
    const draw = () => {
      const w = Math.max(minWidth, Math.floor(div.clientWidth));
      if (!div.clientWidth || w === last) return;
      last = w;
      div.replaceChildren(render(w));
    };
    new ResizeObserver(draw).observe(div);
    requestAnimationFrame(draw);
    div.redraw = () => { last = 0; draw(); };
    return div;
  }

  // Minimize the number of rows without exceeding the width of container el, keeping the number of
  // items per row as balanced as possible. For 7 items, use 4 + 3; for 6, use 3 + 3.
  function balance(el, items) {
    const row = (xs) => { const r = document.createElement("div"); r.className = "row"; r.append(...xs); return r; };
    const split = (rows) => {
      const n = items.length, size = Math.floor(n / rows), extra = n % rows;
      let i = 0;
      return d3.range(rows).map((r) => items.slice(i, (i += size + (r < extra ? 1 : 0))));
    };
    el.classList.add("balanced");
    el.replaceChildren(row(items));
    let lastWidth = 0, lastRows = 1;
    new ResizeObserver(() => {
      const width = el.clientWidth;
      if (!width || width === lastWidth) return;
      lastWidth = width;
      const gap = parseFloat(getComputedStyle(el.firstChild).columnGap) || 0;
      const widths = new Map(items.map((s) => [s, s.getBoundingClientRect().width]));
      const fits = (xs) => d3.sum(xs, (s) => widths.get(s)) + gap * (xs.length - 1) <= width + 0.5;
      let rows = 1;
      while (rows < items.length && !split(rows).every(fits)) rows++;
      if (rows === lastRows) return;
      lastRows = rows;
      el.replaceChildren(...split(rows).map(row));
    }).observe(el);
    return el;
  }

  // Legend: each item uses the { label, color, key } format, where key is "line", "square", or "band".
  function legend(items) {
    return balance(htl.html`<div class="legend"></div>`, items.map((d) =>
      htl.html`<span><i class=${`key key-${d.key ?? "square"}`} style=${`--c:${d.color}`}></i>${d.label}</span>`));
  }

  function figure({ number, title, legendItems, body, note, table }) {
    return htl.html`<figure class="fig">
      <figcaption class="fig-head"><span class="fig-num">Figure ${number}</span> ${title}</figcaption>
      ${legendItems ? legend(legendItems) : null}
      ${body}
      ${note ? htl.html`<p class="fig-note">${note}</p>` : null}
      ${table ?? null}
    </figure>`;
  }

  function tableView(rows, columns, summary = "Show the data") {
    return htl.html`<details class="table-view"><summary>${summary}</summary>
      <div class="table-scroll" tabindex="0" role="region" aria-label=${summary}><table>
        <thead><tr>${columns.map((c) => htl.html`<th scope="col" class=${c.num ? "num" : ""}>${c.label}</th>`)}</tr></thead>
        <tbody>${rows.map((r) => htl.html`<tr>${columns.map((c) =>
          htl.html`<td class=${c.num ? "num" : ""}>${c.format ? c.format(r[c.key], r) : r[c.key]}</td>`)}</tr>`)}</tbody>
      </table></div>
    </details>`;
  }

  // Map tooltips, using the same style as Plot.
  let tipEl = null;
  function tip() {
    if (!tipEl) {
      tipEl = document.createElement("div");
      tipEl.className = "tip";
      tipEl.hidden = true;
      tipEl.setAttribute("aria-hidden", "true");
      document.body.appendChild(tipEl);
    }
    return {
      show(lines, x, y) {
        tipEl.replaceChildren(...lines.map((l, i) => {
          const el = document.createElement(i === 0 ? "strong" : "div");
          el.textContent = l;
          return el;
        }));
        tipEl.hidden = false;
        const r = tipEl.getBoundingClientRect();
        tipEl.style.left = `${Math.min(x + 14, window.innerWidth - r.width - 8)}px`;
        tipEl.style.top = `${y + r.height + 18 > window.innerHeight ? y - r.height - 12 : y + 14}px`;
      },
      hide() { tipEl.hidden = true; }
    };
  }

  // Set a row for each year from the first to the last, and set years without a survey (2020 and
  // 2021) to NaN to break lines and filled areas across this gap.
  function everyYear(rows, keys) {
    const byYear = new Map(rows.map((d) => [d.year, d]));
    const [y0, y1] = d3.extent(rows, (d) => d.year);
    return d3.range(y0, y1 + 1).map((year) => byYear.get(year) ?? { year, ...Object.fromEntries(keys.map((k) => [k, NaN])) });
  }

  const tint = (color, t) => d3.interpolateRgb(PAPER, color)(t);
  const yearAxis = (extra = {}) => ({ label: null, tickFormat: "d", ticks: X_YEARS, tickSize: 0, tickPadding: 8, ...extra });
  const crosshair = (rows, extra = {}) => Plot.ruleX(rows, Plot.pointerX({ x: "year", stroke: INK, strokeOpacity: 0.25, ...extra }));

  // ---------- Figure 1: May pond counts and numbers of breeding dabbling ducks stacked by species ----------

  const STACK = ["NOPI", "MALL", "BWTE", "GADW", "NSHO"]; // From bottom to top

  function pondsAndDucks({ totals, width }) {
    const surveyed = d3.groups(totals, (d) => d.year).map(([year, v]) => {
      const r = { year, ponds: v[0].prairie_ponds };
      for (const sp of STACK) r[sp] = v.find((d) => d.species === sp).prairie_total;
      r.total = d3.sum(STACK, (sp) => r[sp]);
      return r;
    }).sort((a, b) => a.year - b.year);
    const rows = everyYear(surveyed, ["ponds", "total", ...STACK]);
    // Plot periods of consecutive dry springs behind the data, with lines marking each boundary.
    const dry = [];
    for (const year of driestYears(totals)) {
      const last = dry.at(-1);
      if (last && year === last.x2 + 0.5) last.x2 = year + 0.5;
      else dry.push({ x1: year - 0.5, x2: year + 0.5 });
    }
    const domain = [d3.min(rows, (d) => d.year) - 0.5, d3.max(rows, (d) => d.year) + 0.5];
    const common = { width, marginLeft: 36, marginRight: 8, x: yearAxis({ domain }) };
    const dryBands = (top) => [
      Plot.rectY(dry, { x1: "x1", x2: "x2", y1: 0, y2: top, fill: DRY, fillOpacity: 0.45 }),
      Plot.ruleX(dry.flatMap((d) => [d.x1, d.x2]), { y1: 0, y2: top, stroke: DRY, strokeWidth: 2 })
    ];

    const pondTop = 8e6;
    const ponds = plot({
      ...common, height: 166, marginTop: 26, marginBottom: 22,
      x: { ...common.x, axis: null },
      y: { domain: [0, pondTop], ticks: [0, 2e6, 4e6, 6e6, 8e6], tickFormat: (v) => v / 1e6, tickSize: 0, label: "May ponds (millions)", labelArrow: "none" },
      marks: [
        Plot.gridY({ ticks: [2e6, 4e6, 6e6, 8e6], stroke: GRID, strokeOpacity: 1 }),
        dryBands(pondTop),
        Plot.areaY(rows, { x: "year", y1: 0, y2: "ponds", fill: PONDS, fillOpacity: 0.1 }),
        Plot.lineY(rows, { x: "year", y: "ponds", stroke: PONDS, strokeWidth: 2 }),
        Plot.ruleY([0], { stroke: "#c9c5bb" }),
        crosshair(surveyed),
        Plot.tip(surveyed, Plot.pointerX({ x: "year", y: "ponds", ...TIP, title: (d) => `${d.year}\n${fmt.millions(d.ponds, 2)} May ponds` }))
      ]
    });

    const duckTop = Math.ceil(d3.max(rows, (d) => d.total) / 5e6) * 5e6;
    const layers = STACK.map((sp, i) => rows.map((r) => {
      const y1 = STACK.slice(0, i).reduce((s, k) => s + r[k], 0);
      return { year: r.year, y1, y2: y1 + r[sp] };
    }));
    const ducks = plot({
      ...common, height: 250, marginTop: 30, marginBottom: 28,
      y: { domain: [0, duckTop], tickFormat: (v) => v / 1e6, tickSize: 0, label: "Breeding dabbling ducks (millions)", labelArrow: "none" },
      marks: [
        Plot.gridY({ stroke: GRID, strokeOpacity: 1 }),
        dryBands(duckTop),
        ...STACK.map((sp, i) => Plot.areaY(layers[i], { x: "year", y1: "y1", y2: "y2", fill: tint(COLORS[sp], 0.85) })),
        // Thin lines matching the paper colour separate the bands.
        ...STACK.slice(0, -1).map((sp, i) => Plot.lineY(layers[i], { x: "year", y: "y2", stroke: PAPER, strokeWidth: 1.5 })),
        Plot.ruleY([0], { stroke: "#c9c5bb" }),
        crosshair(surveyed),
        Plot.tip(surveyed, Plot.pointerX({
          x: "year", y: "total", ...TIP,
          title: (d) => [`${d.year}: ${fmt.millions(d.total, 2)} ducks`, ...[...STACK].reverse().map((sp) => `${NAMES[sp]}  ${fmt.approx(d[sp])}`)].join("\n")
        }))
      ]
    });
    const wrap = document.createElement("div");
    wrap.append(ponds, ducks);
    return wrap;
  }

  function driestYears(totals, n = 10) {
    return d3.groups(totals, (d) => d.year).map(([year, v]) => ({ year, ponds: v[0].prairie_ponds }))
      .sort((a, b) => a.ponds - b.ponds).slice(0, n).map((d) => d.year).sort((a, b) => a - b);
  }

  // ---------- Figure 2: Recent pond elasticity by species ----------

  function elasticityDots({ recent, width }) {
    const rows = recent.map((d) => ({ ...d, name: NAMES[d.species], color: COLORS[d.species] }))
      .sort((a, b) => b.es_med - a.es_med);
    return plot({
      width, height: 40 * rows.length + 56, marginLeft: Math.min(150, width * 0.34), marginRight: 48, marginTop: 8, marginBottom: 44,
      x: { domain: [0, 1.7], ticks: [0, 0.5, 1, 1.5], tickFormat: (v) => fmt.dec1(v), tickSize: 0, label: "Pond elasticity, last ten surveys", labelAnchor: "center", labelArrow: "none", labelOffset: 36 },
      y: { domain: rows.map((d) => d.name), tickSize: 0, tickPadding: 10, label: null },
      marks: [
        Plot.gridX({ ticks: [0.5, 1.5], stroke: GRID, strokeOpacity: 1 }),
        Plot.ruleX([0], { stroke: "#c9c5bb" }),
        Plot.ruleX([1], { stroke: "#a8a397" }),
        Plot.ruleY(rows, { y: "name", x1: "es_lo", x2: "es_hi", stroke: "color", strokeWidth: 2, strokeLinecap: "round" }),
        Plot.dot(rows, { y: "name", x: "es_med", r: 5, fill: "color", stroke: PAPER, strokeWidth: 2 }),
        Plot.text(rows, { y: "name", x: "es_hi", dx: 10, textAnchor: "start", fill: INK2, text: (d) => fmt.signed(fmt.pct(up10(d.es_med)), 1) }),
        Plot.tip(rows, Plot.pointerY({
          y: "name", x: "es_med", ...TIP,
          title: (d) => `${d.name}\nElasticity ${fmt.dec2(d.es_med)} (95% interval ${fmt.dec2(d.es_lo)} to ${fmt.dec2(d.es_hi)})\n${fmt.pct(up10(d.es_med))} more birds if ponds rise 10%`
        }))
      ]
    });
  }

  // ---------- Figure 3: stratum map showing one species at a time ----------

  const MAP_BREAKS = [0.25, 0.5, 0.75, 1, 1.25];

  // Light-to-dark ramp in one species' colour.
  function ramp(color, n = MAP_BREAKS.length + 1) {
    const black = color === OKABE_ITO.black;
    const stops = [d3.interpolateLab(PAPER, color)(0.13), black ? "#707070" : color, black ? "#000" : d3.interpolateLab(color, "#000")(0.42)];
    return d3.quantize(d3.piecewise(d3.interpolateLab, stops), n);
  }

  function rampLegend(colors, breaks, format) {
    const w = 44;
    return htl.html`<div class="ramp-legend" aria-hidden="true">
      <svg width=${w * colors.length} height="28">
        ${colors.map((c, i) => htl.svg`<rect x=${i * w} y="0" width=${w - 2} height="10" fill=${c} />`)}
        ${breaks.map((b, i) => htl.svg`<text x=${(i + 1) * w - 1} y="24" text-anchor="middle">${format(b)}</text>`)}
      </svg>
    </div>`;
  }

  function speciesMap({ geo, strata, value = "NOPI" }) {
    const W = 680, pad = 12, top = 28;
    const prairie = geo.strata.features.filter((f) => f.properties.prairie);
    const focus = { type: "FeatureCollection", features: prairie };

    // Canada is slightly darker than the United States in the map and its inset.
    const land = (f) => (f.properties.admin === "Canada" ? "#dcd7cb" : "#ebe8e0");

    // Inset map of Canada and the contiguous 48 United States in the upper-right corner.
    const LW = 132;
    const locFeatures = { type: "FeatureCollection", features: geo.locator.features };
    const locProjection = d3.geoIdentity().reflectY(true).fitWidth(LW - 8, locFeatures);
    const [[, ly0], [, ly1]] = d3.geoPath(locProjection).bounds(locFeatures);
    const LH = Math.ceil(ly1 - ly0 + 8);
    locProjection.fitExtent([[4, 4], [LW - 4, LH - 4]], locFeatures);

    // The strata fill 88% of the width, centred, and sit low enough that the
    // small map clears the strata (and their province label) beneath it.
    const projection = d3.geoIdentity().reflectY(true).fitWidth((W - 2 * pad) * 0.88, focus);
    const [[fx0, fy0], [fx1, fy1]] = d3.geoPath(projection).bounds(focus);
    const left = (W - (fx1 - fx0)) / 2;
    const locLeft = W - pad - LW;
    const under = prairie.map((f) => d3.geoPath(projection).bounds(f)).filter((b) => b[1][0] - fx0 + left > locLeft - 8);
    const clearance = d3.min(under, (b) => b[0][1] - fy0) ?? Infinity;
    const strataTop = Math.max(top, pad + LH + 30 - clearance);
    const H = Math.ceil(strataTop + fy1 - fy0 + pad);
    const [tx, ty] = projection.translate();
    projection.translate([tx - fx0 + left, ty - fy0 + strataTop]);
    const path = d3.geoPath(projection);
    const tt = tip();
    const byKey = d3.index(strata, (d) => d.species, (d) => d.stratum);
    const province = (s) => PROVINCES[prairie.find((f) => f.properties.stratum === s).properties.province];
    let sp = value, colors = ramp(COLORS[sp]), scale = d3.scaleThreshold(MAP_BREAKS, colors);

    const svg = d3.create("svg").attr("viewBox", `0 0 ${W} ${H}`).attr("width", "100%").style("overflow", "hidden")
      .attr("role", "group").attr("aria-label", "Map of the 15 Prairie Canada survey strata, shaded by pond elasticity");
    svg.append("g").selectAll("path").data(geo.provinces.features).join("path")
      .attr("d", path).attr("fill", land).attr("stroke", "#fff").attr("stroke-width", 1.5);
    svg.append("g").selectAll("path").data(geo.lakes.features).join("path")
      .attr("d", path).attr("fill", tint(PONDS, 0.16));
    svg.append("g").attr("pointer-events", "none").selectAll("text")
      .data(d3.groups(prairie, (f) => f.properties.province)).join("text")
      .attr("x", ([, fs]) => d3.mean(fs, (f) => path.centroid(f)[0]))
      .attr("y", ([, fs]) => d3.min(fs, (f) => path.bounds(f)[0][1]) - 8)
      .attr("text-anchor", "middle").attr("fill", MUTED)
      .style("font", "500 12.5px var(--sans)").text(([p]) => PROVINCES[p]);

    const paths = svg.append("g").selectAll("path").data(prairie).join("path")
      .attr("class", "stratum").attr("d", path).attr("stroke", PAPER).attr("stroke-width", 1.5)
      .attr("tabindex", 0).attr("role", "img");
    // The keyboard focus ring is placed in a separate layer above the strata.
    const ring = svg.append("path").attr("pointer-events", "none").attr("fill", "none")
      .attr("stroke", INK).attr("stroke-width", 2.5).attr("display", "none");
    const labels = svg.append("g").attr("pointer-events", "none").selectAll("text").data(prairie).join("text")
      .attr("x", (f) => path.centroid(f)[0]).attr("y", (f) => path.centroid(f)[1] + 4)
      .attr("text-anchor", "middle").style("font", "500 11px var(--sans)")
      .text((f) => f.properties.stratum);

    // Inset map with a box indicating the extent shown in the main map.
    const locator = svg.append("g").attr("transform", `translate(${locLeft},${pad})`).attr("aria-hidden", "true");
    locator.append("rect").attr("width", LW).attr("height", LH).attr("rx", 3)
      .attr("fill", "#fff").attr("stroke", "#d6d3ca");
    locator.append("g").selectAll("path").data(locFeatures.features).join("path")
      .attr("d", d3.geoPath(locProjection)).attr("fill", land)
      .attr("stroke", "#fff").attr("stroke-width", 0.4);
    locator.append("path").attr("fill", INK).attr("fill-opacity", 0.12).attr("stroke", INK).attr("stroke-width", 1.5)
      .attr("d", `M${[[0, 0], [W, 0], [W, H], [0, H]].map((c) => locProjection(projection.invert(c))).join("L")}Z`);

    const describe = (s) => {
      const r = byKey.get(sp)?.get(s);
      return [`Stratum ${s}, ${province(s)}`, `${NAMES[sp]}: elasticity ${fmt.dec2(r.es_med)}`, `95% interval ${fmt.dec2(r.es_lo)} to ${fmt.dec2(r.es_hi)}`];
    };
    paths
      .on("pointermove", (e, f) => tt.show(describe(f.properties.stratum), e.clientX, e.clientY))
      .on("pointerleave", () => tt.hide())
      .on("focus", (e, f) => {
        const b = e.target.getBoundingClientRect();
        tt.show(describe(f.properties.stratum), b.right, b.top);
        if (e.target.matches(":focus-visible")) ring.attr("d", path(f)).attr("display", null);
      })
      .on("blur", () => { tt.hide(); ring.attr("display", "none"); });

    const legendSlot = htl.html`<div></div>`;
    const tableSlot = htl.html`<div></div>`;
    function paint() {
      colors = ramp(COLORS[sp]);
      scale = d3.scaleThreshold(MAP_BREAKS, colors);
      const fill = (f) => scale(byKey.get(sp).get(f.properties.stratum).es_med);
      paths.attr("fill", fill).attr("aria-label", (f) => describe(f.properties.stratum).join(", "));
      labels.attr("fill", (f) => (d3.lab(fill(f)).l < 58 ? "#fff" : INK));
      legendSlot.replaceChildren(rampLegend(colors, MAP_BREAKS, (v) => fmt.dec2(v)));
      const wasOpen = tableSlot.querySelector("details")?.open ?? false;
      tableSlot.replaceChildren(tableView(
        prairie.map((f) => byKey.get(sp).get(f.properties.stratum)).sort((a, b) => a.stratum - b.stratum),
        [
          { key: "stratum", label: "Stratum" },
          { key: "stratum", label: "Province", format: province },
          { key: "es_med", label: "Elasticity", num: true, format: fmt.dec2 },
          { key: "es_lo", label: "95% interval", num: true, format: (v, r) => `${fmt.dec2(r.es_lo)} to ${fmt.dec2(r.es_hi)}` }
        ],
        `Show the data for ${NAMES[sp]}`
      ));
      tableSlot.querySelector("details").open = wasOpen;
    }

    const chips = balance(htl.html`<div class="chips" role="radiogroup" aria-label="Species shown on the map"></div>`,
      SPECIES.map((s) => htl.html`<label class="chip"><input type="radio" name="map-species" value=${s} checked=${s === sp}><i style=${`--c:${COLORS[s]}`}></i>${NAMES[s]}</label>`));
    chips.addEventListener("change", (e) => { sp = e.target.value; paint(); });
    paint();

    return figure({
      number: 3,
      title: "Pond elasticity by survey stratum",
      body: htl.html`<div>${chips}<div class="map">${svg.node()}</div>${legendSlot}</div>`,
      note: "Sustained pond elasticity for each stratum, averaged across the most recent ten surveys (median of 300 model draws). Move the pointer over each stratum or navigate using the Tab key to display the value and 95% interval. The inset shows the location of the mapped area within Canada and the United States.",
      table: tableSlot
    });
  }

  // ---------- Figure 4: pond elasticity over time, with one panel per species ----------

  function elasticityTrends({ curves, shift, order }) {
    const main = new Map(shift.filter((d) => d.model === "main").map((d) => [d.species, d]));
    const yDomain = [Math.min(-0.25, d3.min(curves, (d) => d.lo)), Math.max(1.75, d3.max(curves, (d) => d.hi))];
    const panels = order.map((sp) => {
      const surveyed = curves.filter((d) => d.species === sp);
      const rows = everyYear(surveyed, ["lo", "med", "hi"]);
      const s = main.get(sp);
      const chart = responsive((w) => plot({
        width: w, height: 150, marginLeft: 30, marginRight: 6, marginTop: 8, marginBottom: 24,
        x: yearAxis({ domain: [1955, 2026], ticks: [1960, 1990, 2020] }),
        y: { domain: yDomain, ticks: [0, 0.5, 1, 1.5], tickFormat: (v) => fmt.dec1(v), tickSize: 0, label: null },
        marks: [
          Plot.gridY({ ticks: [0.5, 1.5], stroke: GRID, strokeOpacity: 1 }),
          Plot.ruleY([0], { stroke: "#c9c5bb" }),
          Plot.ruleY([1], { stroke: "#a8a397" }),
          Plot.areaY(rows, { x: "year", y1: "lo", y2: "hi", fill: COLORS[sp], fillOpacity: 0.14 }),
          Plot.lineY(rows, { x: "year", y: "med", stroke: COLORS[sp], strokeWidth: 2 }),
          crosshair(surveyed),
          Plot.tip(surveyed, Plot.pointerX({ x: "year", y: "med", ...TIP, title: (d) => `${d.year}\nElasticity ${fmt.dec2(d.med)} (${fmt.dec2(d.lo)} to ${fmt.dec2(d.hi)})` }))
        ]
      }), 150);
      return htl.html`<div class="panel">
        <p class="panel-head"><i class="key key-square" style=${`--c:${COLORS[sp]}`}></i>${NAMES[sp]}<span>${fmt.dec2(s.early_med)} → ${fmt.dec2(s.late_med)}</span></p>
        ${chart}
      </div>`;
    });
    return htl.html`<div class="small-multiples">${panels}</div>`;
  }

  // ---------- Figure 5: breeding birds per May pond ----------

  function perPond({ totals, width, species = ["MALL", "NOPI"] }) {
    const surveyed = totals.filter((d) => species.includes(d.species))
      .map((d) => ({ species: d.species, year: d.year, per: d.prairie_total / d.prairie_ponds }));
    const lines = species.map((sp) => everyYear(surveyed.filter((d) => d.species === sp), ["per"]).map((d) => ({ ...d, species: sp })));
    const last = species.map((sp) => surveyed.filter((d) => d.species === sp).at(-1));
    return plot({
      width, height: 260, marginLeft: 36, marginRight: 128, marginTop: 30, marginBottom: 28,
      x: yearAxis({ domain: [1955, 2026] }),
      y: { domain: [0, Math.ceil(d3.max(surveyed, (d) => d.per) * 2) / 2], tickSize: 0, tickFormat: (v) => fmt.dec1(v), label: "Breeding birds per May pond", labelArrow: "none" },
      marks: [
        Plot.gridY({ stroke: GRID, strokeOpacity: 1 }),
        Plot.ruleY([0], { stroke: "#c9c5bb" }),
        ...lines.map((rows, i) => Plot.lineY(rows, { x: "year", y: "per", stroke: COLORS[species[i]], strokeWidth: 2 })),
        Plot.dot(last, { x: "year", y: "per", r: 4, fill: (d) => COLORS[d.species], stroke: PAPER, strokeWidth: 2 }),
        Plot.text(last, { x: "year", y: "per", dx: 9, textAnchor: "start", fill: INK2, lineHeight: 1.2, text: (d) => `${NAMES[d.species]}\n${fmt.dec2(d.per)} per pond` }),
        Plot.tip(surveyed, Plot.pointer({ x: "year", y: "per", ...TIP, title: (d) => `${NAMES[d.species]}, ${d.year}\n${fmt.dec2(d.per)} birds per pond` }))
      ]
    });
  }

  // ---------- Wetland-loss calculator: changes in breeding bird numbers by species ----------

  function lossBars({ rows, width }) {
    const data = rows.map((d) => ({ ...d, name: NAMES[d.species], color: COLORS[d.species], m: -d.med, lo: -d.hi, hi: -d.lo }))
      .sort((a, b) => b.m - a.m);
    const top = d3.max(data, (d) => d.hi) || 1;
    return plot({
      width, height: 34 * data.length + 44, marginLeft: Math.min(150, width * 0.34), marginRight: 70, marginTop: 4, marginBottom: 44,
      x: { domain: [0, top * 1.02], tickFormat: (v) => fmt.approx(v), ticks: 3, tickSize: 0, label: "Fewer breeding birds (median and 90% interval)", labelAnchor: "center", labelArrow: "none", labelOffset: 38 },
      y: { domain: data.map((d) => d.name), tickSize: 0, tickPadding: 10, padding: 0.42, label: null },
      marks: [
        Plot.gridX({ ticks: 3, stroke: GRID, strokeOpacity: 1 }),
        Plot.barX(data, { y: "name", x: "m", fill: "color", rx: 2 }),
        Plot.ruleX([0], { stroke: "#c9c5bb" }),
        Plot.ruleY(data, { y: "name", x1: "lo", x2: "hi", stroke: "#000", strokeWidth: 1.5 }),
        // Where the interval overlaps the black bar (Canvasback), the overlapping portion is white.
        Plot.ruleY(data.filter((d) => d.color === OKABE_ITO.black && d.lo < d.m),
          { y: "name", x1: "lo", x2: (d) => Math.min(d.m, d.hi), stroke: "#fff", strokeWidth: 1.5 }),
        Plot.text(data, { y: "name", x: "hi", dx: 8, textAnchor: "start", fill: INK2, text: (d) => fmt.approx(d.m) }),
        Plot.tip(data, Plot.pointerY({ y: "name", x: "m", ...TIP, title: (d) => `${d.name}\n${fmt.approx(d.m)} fewer (90% interval ${fmt.approx(d.lo)} to ${fmt.approx(d.hi)})\n${fmt.pct(d.m / d.baseline)} of the recent average` }))
      ]
    });
  }

  // ---------- Scenario calculations (same calculations as scenario_change() in R/summarise.R) ----------

  function quantiles(values, level = 0.9) {
    const a = (1 - level) / 2;
    const s = Float64Array.from(values).sort();
    return { lo: d3.quantileSorted(s, a), med: d3.quantileSorted(s, 0.5), hi: d3.quantileSorted(s, 1 - a) };
  }

  // For each of the draws, set change = recent birds × ((1 − loss)^e − 1), and sum across survey
  // strata and species.
  function runScenario(payload, { species, strata, loss, sustained }) {
    const nDraws = payload[species[0]]?.lag.length ?? 0;
    const total = new Float64Array(nDraws);
    const bySpecies = [];
    let baseline = 0;
    for (const sp of species) {
      const p = payload[sp];
      const acc = new Float64Array(nDraws);
      let base = 0;
      for (const s of strata) {
        const st = p.strata[s];
        if (!st) continue;
        base += st.bpop_ref;
        for (let i = 0; i < nDraws; i++) {
          const e = st.e[i] + (sustained ? p.lag[i] : 0);
          acc[i] += st.bpop_ref * (Math.pow(1 - loss, e) - 1);
        }
      }
      for (let i = 0; i < nDraws; i++) total[i] += acc[i];
      baseline += base;
      bySpecies.push({ species: sp, baseline: base, ...quantiles(acc) });
    }
    return { baseline, total: quantiles(total), bySpecies };
  }

  return {
    plot, responsive, balance, legend, figure, tableView, driestYears,
    pondsAndDucks, elasticityDots, speciesMap, elasticityTrends, perPond, lossBars,
    runScenario, quantiles
  };
}
