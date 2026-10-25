// Questions start collapsed. A click on an option, or a double click on a
// question, opens its page; a click on a question expands or collapses it.
// Only raising edges are laid out. Constraint edges are drawn after each
// layout, for the hovered option or for every option, ending at the
// option's question when that question is collapsed.
const elements = JSON.parse(document.getElementById('elements').textContent);
const cy = cytoscape({
  container: document.getElementById('graph'),
  elements,
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
const parents = new Map(elements.map(e => [e.data.id, e.data.parent]));
const show = n => { cy.zoom(1); cy.center(n); cy.$(':selected').unselect(); n.select(); };
const focusHash = () => {
  const key = decodeURIComponent(location.hash.slice(1));
  if (!key) return;
  const n = cy.getElementById(key);
  if (n.nonempty()) { show(n); return; }
  const q = cy.getElementById(parents.get(key) || '');
  if (q.nonempty() && expandCollapse.isExpandable(q)) expandCollapse.expand(q, { layoutBy: { ...layout, stop: () => show(cy.getElementById(key)) } });
};
cy.layout({ ...layout, stop: () => { fitReadably(); focusHash(); } }).run();
window.addEventListener('hashchange', focusHash);
window.addEventListener('resize', () => cy.resize());
const open = e => { location.href = e.target.data('href'); };
let everyConstraint = false;
const options = elements.filter(e => e.data.constraints);
const shownNode = key => {
  const n = cy.getElementById(key);
  return n.nonempty() ? n : cy.getElementById(parents.get(key) || '');
};
const drawConstraints = hovered => {
  cy.remove('edge.constraint');
  const drawn = [];
  for (const o of options) for (const [cls, target] of o.data.constraints) {
    if (!everyConstraint && hovered !== o.data.id && hovered !== target) continue;
    const from = shownNode(o.data.id), to = shownNode(target);
    if (from.empty() || to.empty() || from.same(to)) continue;
    drawn.push({ group: 'edges', data: { source: from.id(), target: to.id() }, classes: 'constraint ' + cls });
  }
  cy.add(drawn);
};
cy.on('expandcollapse.beforecollapse expandcollapse.beforeexpand', () => cy.remove('edge.constraint'));
cy.on('layoutstop', () => drawConstraints(null));
cy.on('mouseover', 'node.option', e => drawConstraints(e.target.id()));
cy.on('mouseout', 'node.option', () => drawConstraints(null));
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
