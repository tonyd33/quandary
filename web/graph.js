// Questions start collapsed. A click on an option, or a double click on a
// question, opens its page; a click on a question expands or collapses it.
// Only raising edges are laid out. Constraint edges are drawn after each
// layout, for the hovered option or for every option, ending at the
// option's question when that question is collapsed. A change to the
// space redraws the graph in place, keeping the zoom, the pan and which
// questions are expanded.
const initial = JSON.parse(document.getElementById('elements').textContent);
const cy = cytoscape({
  container: document.getElementById('graph'),
  elements: initial,
  wheelSensitivity: 0.3,
  style: graphStyle()
});
const layout = { name: 'dagre', rankDir: 'LR', nodeSep: 12, rankSep: 70, animate: false, fit: false };
const expandCollapse = cy.expandCollapse({ layoutBy: layout, fisheye: false, animate: false, undoable: false, cueEnabled: false });
expandCollapse.collapseAll({ layoutBy: null });
const padding = 20, minZoom = 0.7;
const fitReadably = () => {
  cy.fit(undefined, padding);
  if (cy.zoom() < minZoom) showTopLeft();
};
const showTopLeft = () => {
  cy.zoom(minZoom);
  const b = cy.elements().boundingBox();
  cy.pan({ x: padding - b.x1 * minZoom, y: padding - b.y1 * minZoom });
};
const indexOf = elements => {
  const constraints = elements.flatMap(e => (e.data.constraints || []).map(([cls, target]) => ({ source: e.data.id, target, cls })));
  const touching = new Map();
  for (const c of constraints) for (const key of [c.source, c.target]) {
    if (!touching.has(key)) touching.set(key, []);
    touching.get(key).push(c);
  }
  return { parents: new Map(elements.map(e => [e.data.id, e.data.parent])), constraints, touching };
};
let indexed = indexOf(initial);
const show = n => { cy.zoom(1); cy.center(n); cy.$(':selected').unselect(); n.select(); };
const focusHash = () => {
  const key = decodeURIComponent(location.hash.slice(1));
  if (!key) return;
  const n = cy.getElementById(key);
  if (n.nonempty()) { show(n); return; }
  const q = cy.getElementById(indexed.parents.get(key) || '');
  if (q.nonempty() && expandCollapse.isExpandable(q)) expandCollapse.expand(q, { layoutBy: { ...layout, stop: () => show(cy.getElementById(key)) } });
};
cy.layout({ ...layout, stop: () => { fitReadably(); focusHash(); } }).run();
window.addEventListener('hashchange', focusHash);
window.addEventListener('resize', () => cy.resize());
const open = e => { location.href = e.target.data('href'); };
let everyConstraint = false;
const shownNode = key => {
  const n = cy.getElementById(key);
  return n.nonempty() ? n : cy.getElementById(indexed.parents.get(key) || '');
};
const drawConstraints = hovered => {
  cy.remove('edge.constraint');
  const drawn = [];
  for (const c of everyConstraint ? indexed.constraints : indexed.touching.get(hovered) || []) {
    const from = shownNode(c.source), to = shownNode(c.target);
    if (from.empty() || to.empty() || from.same(to)) continue;
    drawn.push({ group: 'edges', data: { source: from.id(), target: to.id() }, classes: 'constraint ' + c.cls });
  }
  cy.add(drawn);
};
cy.on('expandcollapse.beforecollapse expandcollapse.beforeexpand', () => cy.remove('edge.constraint'));
cy.on('layoutstop', () => drawConstraints(null));
cy.on('mouseover', 'node.option', e => { if (!everyConstraint) drawConstraints(e.target.id()); });
cy.on('mouseout', 'node.option', () => { if (!everyConstraint) drawConstraints(null); });
document.getElementById('constraints').onclick = e => {
  everyConstraint = !everyConstraint;
  e.target.classList.toggle('on', everyConstraint);
  drawConstraints(null);
};
cy.on('tap', 'node.option', open);
cy.on('dbltap', 'node.question', open);
cy.on('tap', 'node.question', e => { const n = e.target; if (expandCollapse.isExpandable(n)) expandCollapse.expand(n); else if (expandCollapse.isCollapsible(n)) expandCollapse.collapse(n); });
document.getElementById('expand').onclick = () => expandCollapse.expandAll();
document.getElementById('collapse').onclick = () => expandCollapse.collapseAll();
document.getElementById('fit').onclick = () => cy.fit(undefined, padding);

// Redraw the graph with the space's current elements, once for any
// number of changes arriving while it fetches. When the space has
// problems, the page showing them replaces this one.
let updating = false, changedAgain = false;
async function updateGraph() {
  if (updating) { changedAgain = true; return; }
  updating = true;
  try {
    const response = await fetch(location.pathname + '/elements' + location.search);
    if (!response.ok) { location.reload(); return; }
    redraw(await response.json());
  } finally {
    updating = false;
    if (changedAgain) { changedAgain = false; updateGraph(); }
  }
}

// Replace the elements, keeping expanded the questions that were, and lay
// them out at the zoom and pan the view had.
function redraw(elements) {
  const expanded = new Set(cy.nodes('node.question').filter(n => expandCollapse.isCollapsible(n)).map(n => n.id()));
  const zoom = cy.zoom(), pan = cy.pan();
  cy.elements().remove();
  cy.add(elements);
  indexed = indexOf(elements);
  expandCollapse.collapseAll({ layoutBy: null });
  expandCollapse.expand(cy.nodes().filter(n => expanded.has(n.id())), { layoutBy: null });
  cy.layout({ ...layout, stop: () => { cy.zoom(zoom); cy.pan(pan); } }).run();
}
