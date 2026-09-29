// The wetland-loss calculator. All values are calculated in the browser using runScenario() in
// viz.js, based on model draws exported from R (data/site/scenario.json).

import { SPECIES, DABBLERS, NAMES, PLURALS, COLORS, PROVINCES, fmt } from "./viz.js";

const AREAS = { all: "Prairie Canada", AB: "Alberta", SK: "Saskatchewan", MB: "Manitoba" };

export function calculator({ htl, K, geo, payload }) {
  const prairie = geo.strata.features.filter((f) => f.properties.prairie).map((f) => f.properties);
  const strataFor = (area) => prairie.filter((p) => area === "all" || p.province === area).map((p) => p.stratum).sort((a, b) => a - b);
  const state = { loss: 0.1, area: "all", species: new Set(DABBLERS) };

  const lossOut = htl.html`<output>10%</output>`;
  const lossInput = htl.html`<input type="range" min="1" max="50" step="1" value="10" aria-label="Share of May ponds lost">`;
  const areaInput = htl.html`<div class="segmented" role="radiogroup" aria-label="Area">${Object.entries(AREAS).map(([k, label]) => htl.html`<label><input type="radio" name="calc-area" value=${k} checked=${k === state.area}><span>${label}</span></label>`)}</div>`;
  const speciesInput = K.balance(htl.html`<div class="chips" role="group" aria-label="Species"></div>`,
    SPECIES.map((s) => htl.html`<label class="chip"><input type="checkbox" value=${s} checked=${state.species.has(s)}><i style=${`--c:${COLORS[s]}`}></i>${NAMES[s]}</label>`));

  const value = htl.html`<div class="result-value"></div>`;
  const label = htl.html`<div class="result-label"></div>`;
  const sentence = htl.html`<p class="result-sentence"></p>`;
  const empty = htl.html`<p class="result-empty" hidden>Select at least one species.</p>`;
  let result = null;
  const bars = K.responsive((w) => (result ? K.lossBars({ rows: result.bySpecies, width: w }) : document.createElement("div")));

  function who(species) {
    if (species.length === DABBLERS.length && DABBLERS.every((s) => species.includes(s))) return "breeding dabbling ducks";
    if (species.length === 1) return `breeding ${PLURALS[species[0]]}`;
    return "breeding ducks of the selected species";
  }

  function update() {
    const species = SPECIES.filter((s) => state.species.has(s));
    lossOut.textContent = fmt.pct(state.loss);
    empty.hidden = species.length > 0;
    for (const el of [value, label, sentence, bars]) el.hidden = species.length === 0;
    if (!species.length) { result = null; return; }
    result = K.runScenario(payload, { species, strata: strataFor(state.area), loss: state.loss, sustained: true });
    const { lo, med, hi } = result.total;
    const place = state.area === "all" ? "across Prairie Canada" : `in the ${PROVINCES[state.area]} strata`;
    value.textContent = fmt.approx(-med);
    label.textContent = `fewer ${who(species)} each spring`;
    sentence.textContent = `If ${fmt.pct(state.loss)} of May ponds ${place} were permanently lost, the model estimates that approximately ${fmt.approx(-med)} fewer ${who(species)} would settle there in spring each year (90% interval ${fmt.approx(-hi)} to ${fmt.approx(-lo)}). This reduction would represent ${fmt.share(-med / result.baseline)} of the recent mean (${fmt.approx(result.baseline)}).`;
    bars.redraw();
  }

  lossInput.addEventListener("input", () => { state.loss = +lossInput.value / 100; update(); });
  areaInput.addEventListener("change", (e) => { state.area = e.target.value; update(); });
  speciesInput.addEventListener("change", (e) => {
    e.target.checked ? state.species.add(e.target.value) : state.species.delete(e.target.value);
    update();
  });
  update();

  return htl.html`<div class="calculator">
    <div class="calc-controls">
      <div class="control"><span class="control-label">May ponds lost ${lossOut}</span>${lossInput}</div>
      <div class="control"><span class="control-label">Area</span>${areaInput}</div>
      <div class="control"><span class="control-label">Species</span>${speciesInput}</div>
    </div>
    <div class="calc-result" aria-live="polite">
      ${value}${label}${empty}
      ${bars}
      ${sentence}
    </div>
  </div>`;
}
