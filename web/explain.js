// Draw each cause's graph left to right, as tall as it needs up to a
// limit. A click on an option or question opens its page.
for (const holder of document.querySelectorAll('.explanation')) {
  const container = holder.querySelector('.cause-graph');
  const cy = cytoscape({
    container,
    elements: JSON.parse(holder.querySelector('script').textContent),
    style: graphStyle(),
    maxZoom: 1.2,
    wheelSensitivity: 0.3
  });
  cy.layout({
    name: 'dagre', rankDir: 'LR', nodeSep: 14, rankSep: 60, fit: false,
    stop: () => {
      container.style.height = Math.min(cy.elements().boundingBox().h + 40, 480) + 'px';
      cy.resize();
      cy.fit(undefined, 16);
    }
  }).run();
  cy.on('tap', 'node[href]', e => { location.href = e.target.data('href'); });
}
