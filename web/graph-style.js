// Returns the Cytoscape style of the graph and explanation pages, coloured
// by the stylesheet's tokens.
const graphStyle = () => {
  const cssVar = n => getComputedStyle(document.documentElement).getPropertyValue(n).trim();
  return [
    { selector: 'node', style: { label: 'data(label)', color: cssVar('--fg'), 'font-size': 11, 'text-wrap': 'wrap', 'text-max-width': 220, 'text-valign': 'center', shape: 'round-rectangle', width: 'label', height: 'label', padding: 6, 'background-color': cssVar('--bg'), 'border-width': 1, 'border-color': cssVar('--muted') } },
    { selector: 'node.question', style: { 'text-valign': 'top', 'font-weight': 600, 'background-color': cssVar('--panel'), 'border-color': cssVar('--question') } },
    { selector: 'node.question.cy-expand-collapse-collapsed-node', style: { 'text-valign': 'center' } },
    { selector: 'node.dormant', style: { 'border-style': 'dashed', color: cssVar('--impossible') } },
    { selector: 'node.forced', style: { 'background-color': cssVar('--forced-bg'), 'border-color': cssVar('--forced') } },
    { selector: 'node.possible', style: { 'background-color': cssVar('--possible-bg'), 'border-color': cssVar('--possible') } },
    { selector: 'node.impossible', style: { 'background-color': cssVar('--impossible-bg'), 'border-color': cssVar('--impossible'), color: cssVar('--impossible') } },
    { selector: 'node.target', style: { 'border-width': 3 } },
    { selector: 'node.joint', style: { shape: 'ellipse', 'font-size': 9 } },
    { selector: 'node.forbid', style: { shape: 'cut-rectangle', 'border-color': cssVar('--stale'), color: cssVar('--stale') } },
    { selector: 'edge', style: { 'curve-style': 'bezier', 'target-arrow-shape': 'triangle', width: 1, 'line-color': cssVar('--muted'), 'target-arrow-color': cssVar('--muted') } },
    { selector: 'edge.needs, edge.needs-one-of', style: { 'line-color': cssVar('--forced'), 'target-arrow-color': cssVar('--forced'), width: 1.5 } },
    { selector: 'edge.needs-one-of', style: { 'line-style': 'dotted' } },
    { selector: 'edge.excludes', style: { 'line-style': 'dashed', 'line-color': cssVar('--stale'), 'target-arrow-color': cssVar('--stale'), 'target-arrow-shape': 'tee' } },
    { selector: 'edge.forbids', style: { 'line-color': cssVar('--stale'), 'target-arrow-color': cssVar('--stale'), 'target-arrow-shape': 'tee', 'font-size': 9, color: cssVar('--stale') } },
    { selector: 'edge.forbids.taking', style: { label: 'taking' } },
    { selector: 'edge.forbids.refusing', style: { label: 'refusing' } },
    { selector: ':selected', style: { 'border-width': 3 } }
  ];
};
