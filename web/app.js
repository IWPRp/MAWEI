/* MAWEI web dashboard
 *
 * Renders Sankeys in the browser from the JSON the R pipeline emits, rather than embedding
 * pre-rendered widgets. A self-contained plotly HTML is 1-4 MB because it carries the whole
 * library; the JSON is 20-80 KB and the library comes once from a CDN. More importantly the
 * JSON carries values per year, so one file serves the whole animation and any unit
 * conversion, and node coordinates and colours come from the same R functions that draw the
 * static diagrams — the two views cannot drift apart.
 */

const DATA = 'data';
const YEARS = [2020, 2021, 2022, 2023, 2024];

// Unit conversions offered per system. Values are stored in the base unit; the factor is
// applied at draw time so the underlying figures are never mutated.
const UNITS = {
  MGD: [
    { label: 'MGD',    unit: 'MGD',    factor: 1 },
    { label: 'MG/yr',  unit: 'MG/yr',  factor: 365 },
    { label: 'af/yr',  unit: 'ac-ft/yr', factor: 365 * 3.06889 }
  ],
  PJ: [
    { label: 'PJ',  unit: 'PJ',  factor: 1 },
    { label: 'TWh', unit: 'TWh', factor: 1 / 3.6 },
    { label: 'TBtu', unit: 'TBtu', factor: 0.947817 }
  ]
};

const state = {
  domain: 'water',
  county: null,        // null = metro
  variant: 'full',     // energy-water only
  year: 2024,
  unitIdx: 0,
  playing: false,
  timer: null,
  cache: new Map()
};

let manifest = null;

/* ---------------- data access ---------------- */

// Artefact stems, mirroring the R naming convention. Derived from the manifest where
// possible so a rename in R propagates without editing this file.
function stemFor(domain, county, variant) {
  const m = manifest.files.filter(f =>
    f.domain === domain && f.kind === 'data' &&
    (county ? f.county === county : f.scope === 'metro'));
  if (m.length === 0) return null;
  if (domain !== 'energy-water') return m[0].path;
  // energy-water has a full and a simplified cut at both scopes
  const want = variant === 'simplified' ? /simplified/ : /^(?!.*simplified)/;
  const hit = m.find(f => want.test(f.path));
  return (hit || m[0]).path;
}

async function load(path) {
  if (state.cache.has(path)) return state.cache.get(path);
  const r = await fetch(`${DATA}/${path}`);
  if (!r.ok) throw new Error(`${path}: ${r.status}`);
  const j = await r.json();
  state.cache.set(path, j);
  return j;
}

/* ---------------- rendering ---------------- */

function unitsFor(diagram) {
  const u = Array.isArray(diagram.meta.units) ? diagram.meta.units[0] : diagram.meta.units;
  return UNITS[u] ? u : (u === 'auto' ? null : null);
}

function fmt(v) {
  const a = Math.abs(v);
  if (a >= 1000) return v.toLocaleString(undefined, { maximumFractionDigits: 0 });
  if (a >= 100)  return v.toFixed(0);
  if (a >= 10)   return v.toFixed(1);
  if (a >= 1)    return v.toFixed(2);
  return v.toPrecision(2);
}

// Build the plotly trace for one year. Node coordinates and colours come straight from the
// JSON, so the browser does no layout of its own and the picture is stable across years.
function trace(diagram, year, conv) {
  const links = diagram.links.filter(l => l.year === year && l.value > 0);

  // Only label nodes that carry flow this year: a label on an empty node reads as a
  // measurement of zero rather than as an absence.
  const active = new Set();
  links.forEach(l => { active.add(l.s); active.add(l.t); });

  const totals = new Map();
  links.forEach(l => {
    totals.set(l.t, (totals.get(l.t) || 0) + l.value);
    totals.set(l.s, (totals.get(l.s) || 0) + l.value);
  });
  // a node's throughput is the larger of what enters and what leaves it
  const inflow = new Map(), outflow = new Map();
  links.forEach(l => {
    inflow.set(l.t, (inflow.get(l.t) || 0) + l.value);
    outflow.set(l.s, (outflow.get(l.s) || 0) + l.value);
  });

  const labels = diagram.nodes.map(n => {
    if (!active.has(n.id)) return '';
    const v = Math.max(inflow.get(n.id) || 0, outflow.get(n.id) || 0);
    const u = conv ? conv.unit : '';
    return `${n.label}<br>${fmt(v * (conv ? conv.factor : 1))} ${u}`;
  });

  return {
    type: 'sankey',
    arrangement: 'snap',
    valueformat: ',.3~f',
    valuesuffix: conv ? ' ' + conv.unit : '',
    node: {
      label: labels,
      x: diagram.nodes.map(n => n.x),
      y: diagram.nodes.map(n => n.y),
      color: diagram.nodes.map(n => active.has(n.id) ? n.color : 'rgba(0,0,0,0)'),
      pad: 12,
      thickness: 16,
      hovertemplate: '%{label}<extra></extra>'
    },
    link: {
      source: links.map(l => l.s),
      target: links.map(l => l.t),
      value: links.map(l => l.value * (conv ? conv.factor : 1)),
      color: links.map(l => l.color),
      hovertemplate: '%{source.label} → %{target.label}<br>%{value}<extra></extra>'
    }
  };
}

function layoutFor() {
  const dark = document.documentElement.dataset.theme === 'dark';
  return {
    font: { size: 11.5, color: dark ? '#e6ecf2' : '#1f2933',
            family: '-apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif' },
    paper_bgcolor: 'rgba(0,0,0,0)',
    plot_bgcolor: 'rgba(0,0,0,0)',
    margin: { l: 8, r: 8, t: 10, b: 8 }
  };
}

async function draw() {
  const el = document.getElementById('chart');
  const path = stemFor(state.domain, state.county, state.variant);
  if (!path) { el.innerHTML = '<p class="loading">No diagram for this selection.</p>'; return; }

  let d;
  try { d = await load(path); }
  catch (e) { el.innerHTML = `<p class="loading">Could not load ${path}</p>`; return; }

  const base = unitsFor(d);
  buildUnitSeg(base);
  const conv = base ? UNITS[base][state.unitIdx] : null;

  Plotly.react(el, [trace(d, state.year, conv)], layoutFor(),
               { displaylogo: false, responsive: true,
                 modeBarButtonsToRemove: ['lasso2d', 'select2d'] });

  updateTitle(d, conv);
  updateStats(d, conv);
  syncUrl();
}

function updateTitle(d, conv) {
  const where = state.county || 'Metro Atlanta';
  const what = { water: 'water flows', energy: 'energy flows',
                 'energy-water': 'water and energy flows' }[state.domain];
  document.getElementById('chart-title').textContent = `${where} — ${what}`;
  document.getElementById('scope-label').textContent = state.county || 'Metro total';

  const u = conv ? conv.unit : 'mixed units';
  document.getElementById('caption').textContent =
    `Ribbon width is proportional to volume, in ${u}. Losses and waste sinks are grouped at ` +
    `the top of the right-hand column. Nodes carrying no flow in ${state.year} are hidden ` +
    `rather than drawn at zero.`;
}

function updateStats(d, conv) {
  const el = document.getElementById('stats');
  const links = d.links.filter(l => l.year === state.year && l.value > 0);

  // Sources are nodes that never appear as a target; sinks never appear as a source. Both
  // are properties of the graph, so they hold for any diagram without special-casing.
  const asTarget = new Set(links.map(l => l.t));
  const asSource = new Set(links.map(l => l.s));

  // A combined diagram carries water in MGD and energy in PJ. Those cannot be added, so
  // report each system separately rather than a single meaningless total.
  const systems = [...new Set(links.map(l => l.units))].filter(Boolean);
  const rows = systems.map(u => {
    const sub = links.filter(l => l.units === u);
    const f = (conv && conv.unit === u) ? conv.factor : 1;
    const shown = (conv && conv.unit === u) ? conv.unit : u;
    const subTarget = new Set(sub.map(l => l.t));
    const sourceIds = [...new Set(sub.map(l => l.s))].filter(i => !subTarget.has(i));
    const input = sub.filter(l => sourceIds.includes(l.s))
                     .reduce((a, l) => a + l.value, 0) * f;
    const lossIds = d.nodes.filter(n => /loss|rejected|septic|own use/i.test(n.label))
                           .map(n => n.id);
    const loss = sub.filter(l => lossIds.includes(l.t))
                    .reduce((a, l) => a + l.value, 0) * f;
    return `<dt>${u === 'MGD' ? 'Water in' : 'Energy in'}</dt><dd>${fmt(input)} ${shown}</dd>
            <dt>… to losses</dt><dd>${fmt(loss)} ${shown}</dd>
            <dt>… loss share</dt><dd>${input ? (100 * loss / input).toFixed(1) : '–'}%</dd>`;
  }).join('');

  el.innerHTML = `
    <label>${state.year} totals</label>
    <dl>
      ${rows}
      <dt>Flows drawn</dt><dd>${links.length}</dd>
      <dt>Nodes</dt><dd>${new Set([...asSource, ...asTarget]).size}</dd>
    </dl>`;
}

function buildUnitSeg(base) {
  const seg = document.getElementById('unit-seg');
  if (!base) { seg.innerHTML = '<button class="active">mixed</button>'; return; }
  if (seg.dataset.base === base) {
    [...seg.children].forEach((b, i) => b.classList.toggle('active', i === state.unitIdx));
    return;
  }
  seg.dataset.base = base;
  seg.innerHTML = UNITS[base]
    .map((u, i) => `<button data-i="${i}" class="${i === state.unitIdx ? 'active' : ''}">${u.label}</button>`)
    .join('');
  seg.querySelectorAll('button').forEach(b => b.addEventListener('click', () => {
    state.unitIdx = +b.dataset.i;
    draw();
  }));
}

/* ---------------- county map ---------------- */
// A schematic grid rather than true geography: at this size real polygons are unreadable,
// and the only job here is to let a county be picked and to show which is active. Positions
// approximate the metro's actual arrangement so the shape is still recognisable.
const GRID = {
  Cherokee: [1, 0], Forsyth: [2, 0], Hall: [3, 0],
  Bartow: [0, 1], Cobb: [1, 1], Gwinnett: [3, 1],
  Paulding: [0, 2], Fulton: [1.5, 2], DeKalb: [2.5, 2], Rockdale: [3.5, 2],
  Douglas: [0.5, 3], Clayton: [2, 3], Henry: [3, 3],
  Coweta: [1, 4], Fayette: [2, 4]
};

function drawMap() {
  const svg = document.getElementById('map');
  const cw = 62, ch = 52, pad = 6;
  svg.setAttribute('viewBox', `0 0 ${4.5 * cw + pad} ${5 * ch + pad}`);
  svg.innerHTML = Object.entries(GRID).map(([name, [cx, cy]]) => {
    const x = cx * cw + pad, y = cy * ch + pad;
    const on = state.county === name;
    return `<g data-county="${name}">
      <rect x="${x}" y="${y}" width="${cw - 4}" height="${ch - 4}" rx="4"
        fill="${on ? 'var(--accent)' : 'var(--accent-soft)'}"
        stroke="var(--line)" stroke-width="1"/>
      <text x="${x + (cw - 4) / 2}" y="${y + (ch - 4) / 2 + 2.5}"
        fill="${on ? '#fff' : 'currentColor'}">${name}</text>
    </g>`;
  }).join('');
  svg.querySelectorAll('g').forEach(g => g.addEventListener('click', () => {
    const n = g.dataset.county;
    state.county = state.county === n ? null : n;
    drawMap(); draw();
  }));
}

/* ---------------- compare view ---------------- */

async function drawCompare() {
  const dom = document.getElementById('cmp-domain').value;
  const a = document.getElementById('cmp-a').value || null;
  const b = document.getElementById('cmp-b').value || null;
  const yr = +document.getElementById('cmp-year').value;

  const [da, db] = await Promise.all([
    load(stemFor(dom, a === 'metro' ? null : a, 'full')),
    load(stemFor(dom, b === 'metro' ? null : b, 'full'))
  ]);
  const base = unitsFor(da);
  const conv = base ? UNITS[base][0] : null;

  document.getElementById('cmp-a-title').textContent = (a === 'metro' ? 'Metro Atlanta' : a);
  document.getElementById('cmp-b-title').textContent = (b === 'metro' ? 'Metro Atlanta' : b);

  // A shared value range is what makes the two panels comparable: without it plotly scales
  // each to its own maximum and a small county looks the same size as the metro total.
  const maxOf = d => Math.max(...d.links.filter(l => l.year === yr).map(l => l.value), 0);
  const cap = Math.max(maxOf(da), maxOf(db));
  const opts = { displaylogo: false, responsive: true };
  [[da, 'cmp-chart-a'], [db, 'cmp-chart-b']].forEach(([d, id]) => {
    const t = trace(d, yr, conv);
    // pad the smaller panel with an invisible link at the shared maximum
    t.link.source.push(0); t.link.target.push(0);
    t.link.value.push(cap === 0 ? 0 : cap * 1e-9);
    t.link.color.push('rgba(0,0,0,0)');
    Plotly.react(document.getElementById(id), [t], layoutFor(), opts);
  });
}

function fillCompareControls() {
  const areas = ['metro', ...manifest.counties];
  const opt = v => `<option value="${v}">${v === 'metro' ? 'Metro Atlanta' : v}</option>`;
  document.getElementById('cmp-domain').innerHTML =
    manifest.domains.map(d => `<option value="${d}">${
      { water: 'Water', energy: 'Energy', 'energy-water': 'Water & energy' }[d]}</option>`).join('');
  document.getElementById('cmp-a').innerHTML = areas.map(opt).join('');
  document.getElementById('cmp-b').innerHTML = areas.map(opt).join('');
  document.getElementById('cmp-b').value = 'Fulton';
  document.getElementById('cmp-year').innerHTML =
    manifest.years.map(y => `<option value="${y}">${y}</option>`).join('');
  document.getElementById('cmp-year').value = Math.max(...manifest.years);
  ['cmp-domain', 'cmp-a', 'cmp-b', 'cmp-year'].forEach(id =>
    document.getElementById(id).addEventListener('change', drawCompare));
}

/* ---------------- URL state ---------------- */
// A view is worth citing, so it needs an address. Reading state back on load also means the
// browser's back button moves between selections rather than leaving the site.
function syncUrl() {
  const p = new URLSearchParams();
  p.set('domain', state.domain);
  if (state.county) p.set('county', state.county);
  if (state.domain === 'energy-water' && state.variant !== 'full') p.set('variant', state.variant);
  p.set('year', state.year);
  history.replaceState(null, '', '?' + p);
}

function readUrl() {
  const p = new URLSearchParams(location.search);
  if (p.has('domain')) state.domain = p.get('domain');
  if (p.has('county')) state.county = p.get('county');
  if (p.has('variant')) state.variant = p.get('variant');
  if (p.has('year')) state.year = +p.get('year');
}

/* ---------------- wiring ---------------- */

function wire() {
  document.querySelectorAll('#domain-seg button').forEach(b =>
    b.addEventListener('click', () => {
      state.domain = b.dataset.domain;
      state.unitIdx = 0;
      document.querySelectorAll('#domain-seg button')
        .forEach(x => x.classList.toggle('active', x === b));
      document.getElementById('variant-ctrl').hidden = state.domain !== 'energy-water';
      draw();
    }));

  document.querySelectorAll('#variant-seg button').forEach(b =>
    b.addEventListener('click', () => {
      state.variant = b.dataset.variant;
      document.querySelectorAll('#variant-seg button')
        .forEach(x => x.classList.toggle('active', x === b));
      draw();
    }));

  const yr = document.getElementById('year');
  yr.max = YEARS.length - 1;
  yr.value = YEARS.indexOf(state.year);
  document.getElementById('year-out').textContent = state.year;
  yr.addEventListener('input', () => {
    state.year = YEARS[+yr.value];
    document.getElementById('year-out').textContent = state.year;
    draw();
  });

  document.getElementById('play').addEventListener('click', () => {
    const btn = document.getElementById('play');
    if (state.playing) {
      clearInterval(state.timer); state.playing = false; btn.textContent = '▶';
      return;
    }
    state.playing = true; btn.textContent = '❙❙';
    state.timer = setInterval(() => {
      const i = (YEARS.indexOf(state.year) + 1) % YEARS.length;
      state.year = YEARS[i];
      yr.value = i;
      document.getElementById('year-out').textContent = state.year;
      draw();
    }, 1400);
  });

  document.getElementById('reset-scope').addEventListener('click', () => {
    state.county = null; drawMap(); draw();
  });

  document.querySelectorAll('#tabs .tab').forEach(t =>
    t.addEventListener('click', () => {
      document.querySelectorAll('#tabs .tab').forEach(x => x.classList.remove('active'));
      t.classList.add('active');
      document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
      document.getElementById('view-' + t.dataset.view).classList.add('active');
      if (t.dataset.view === 'compare') drawCompare();
      if (t.dataset.view === 'flows') Plotly.Plots.resize(document.getElementById('chart'));
    }));

  document.getElementById('theme-toggle').addEventListener('click', () => {
    const dark = document.documentElement.dataset.theme === 'dark';
    document.documentElement.dataset.theme = dark ? 'light' : 'dark';
    localStorage.setItem('mawei-theme', dark ? 'light' : 'dark');
    draw();
    if (document.getElementById('view-compare').classList.contains('active')) drawCompare();
  });

  document.getElementById('dl-png').addEventListener('click', () =>
    Plotly.downloadImage(document.getElementById('chart'), {
      format: 'png', scale: 2.5, filename: fileStem() }));

  document.getElementById('dl-json').addEventListener('click', async () => {
    const p = stemFor(state.domain, state.county, state.variant);
    saveBlob(JSON.stringify(await load(p), null, 2), fileStem() + '.json', 'application/json');
  });

  document.getElementById('dl-csv').addEventListener('click', async () => {
    const d = await load(stemFor(state.domain, state.county, state.variant));
    const name = i => d.nodes[i].label;
    const rows = [['year', 'source', 'target', 'value', 'units'],
      ...d.links.map(l => [l.year, name(l.s), name(l.t), l.value, l.units])];
    saveBlob(rows.map(r => r.map(csvCell).join(',')).join('\n'),
             fileStem() + '.csv', 'text/csv');
  });
}

const csvCell = v => /[",\n]/.test(String(v)) ? `"${String(v).replace(/"/g, '""')}"` : v;
const fileStem = () =>
  `mawei_${state.domain}_${state.county || 'metro'}_${state.year}`;

function saveBlob(text, filename, type) {
  const a = document.createElement('a');
  a.href = URL.createObjectURL(new Blob([text], { type }));
  a.download = filename;
  a.click();
  URL.revokeObjectURL(a.href);
}

/* ---------------- boot ---------------- */

(async function init() {
  document.documentElement.dataset.theme = localStorage.getItem('mawei-theme') || 'light';
  readUrl();
  try {
    manifest = await (await fetch(`${DATA}/manifest.json`)).json();
  } catch (e) {
    // Distinguish the two failure modes. Opening index.html from disk is the common one and
    // is not a fault to be fixed: browsers block fetch() on file:// URLs, so the dashboard
    // has to be served. A genuine http failure means the artefacts are missing instead.
    const isFile = location.protocol === 'file:';
    document.getElementById('chart').innerHTML = isFile
      ? '<div class="notice"><h2>Serve this folder to view the dashboard</h2>' +
        '<p>Browsers block local file reads, so the dashboard cannot run from a ' +
        '<code>file://</code> address. Double-click <code>web/serve.command</code>, ' +
        'or run:</p><pre>cd web &amp;&amp; ln -s ../outputs/files data\n' +
        'python3 -m http.server 8787</pre>' +
        '<p>then open <a href="http://localhost:8787/index.html">localhost:8787</a>. ' +
        'The static diagrams in <code>interface/MAWEI.html</code> open directly, with no ' +
        'server needed.</p></div>'
      : '<div class="notice"><h2>Artefact index not found</h2>' +
        '<p>Expected <code>' + DATA + '/manifest.json</code>. Generate it with:</p>' +
        '<pre>Rscript R/flows_energy_water.R</pre></div>';
    return;
  }
  document.getElementById('about-meta').textContent =
    `${manifest.files.length} artefacts, ${manifest.counties.length} counties, ` +
    `${manifest.years[0]}–${manifest.years[manifest.years.length - 1]}. ` +
    `Generated ${manifest.generated.slice(0, 10)}.`;

  document.querySelectorAll('#domain-seg button').forEach(b =>
    b.classList.toggle('active', b.dataset.domain === state.domain));
  document.getElementById('variant-ctrl').hidden = state.domain !== 'energy-water';

  wire();
  drawMap();
  fillCompareControls();
  await draw();
})();
