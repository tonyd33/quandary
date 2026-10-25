// Questions start collapsed. A click on an option, or a double click on a
// question, opens its page; a click on a question expands or collapses it.
const elements = JSON.parse(document.getElementById('elements').textContent);
const cssVar = n => getComputedStyle(document.documentElement).getPropertyValue(n).trim();
const cy = cytoscape({
  container: document.getElementById('graph'),
  elements,
  wheelSensitivity: 0.3,
  style: [
    { selector: 'node', style: { label: 'data(label)', color: cssVar('--fg'), 'font-size': 11, 'text-wrap': 'wrap', 'text-max-width': 220, 'text-valign': 'center', shape: 'round-rectangle', width: 'label', height: 'label', padding: 6, 'background-color': cssVar('--bg'), 'border-width': 1, 'border-color': cssVar('--muted') } },
    { selector: 'node.question', style: { 'text-valign': 'top', 'font-weight': 600, 'background-color': cssVar('--panel'), 'border-color': cssVar('--question') } },
    { selector: 'node.question.cy-expand-collapse-collapsed-node', style: { 'text-valign': 'center' } },
    { selector: 'node.forced', style: { 'background-color': cssVar('--forced-bg'), 'border-color': cssVar('--forced') } },
    { selector: 'node.possible', style: { 'background-color': cssVar('--possible-bg'), 'border-color': cssVar('--possible') } },
    { selector: 'node.impossible', style: { 'background-color': cssVar('--impossible-bg'), 'border-color': cssVar('--impossible'), color: cssVar('--impossible') } },
    { selector: 'node.joint', style: { shape: 'ellipse', 'font-size': 9 } },
    { selector: 'edge', style: { 'curve-style': 'bezier', 'target-arrow-shape': 'triangle', width: 1, 'line-color': cssVar('--muted'), 'target-arrow-color': cssVar('--muted') } },
    { selector: 'edge.needs-one-of', style: { 'line-style': 'dotted' } },
    { selector: 'edge.excludes', style: { 'line-style': 'dashed', 'line-color': cssVar('--stale'), 'target-arrow-color': cssVar('--stale'), 'target-arrow-shape': 'tee' } },
    { selector: ':selected', style: { 'border-width': 3 } }
  ]
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
cy.on('tap', 'node.option', open);
cy.on('dbltap', 'node.question', open);
cy.on('tap', 'node.question', e => { const n = e.target; if (expandCollapse.isExpandable(n)) expandCollapse.expand(n); else if (expandCollapse.isCollapsible(n)) expandCollapse.collapse(n); });
document.getElementById('expand').onclick = () => expandCollapse.expandAll();
document.getElementById('collapse').onclick = () => expandCollapse.collapseAll();
document.getElementById('fit').onclick = () => cy.fit(undefined, padding);
