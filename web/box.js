// Hovering or focusing an option marks the options it needs, needs one
// of and excludes, the options needing or excluding it, and the
// questions it raises; the rest dims.
const cells = [...document.querySelectorAll('.cell')];
const rows = [...document.querySelectorAll('.box-row')];
const keysOf = (el, name) => (el.dataset[name] || '').split(' ').filter(Boolean);
const marks = ['self', 'needs', 'needs-one-of', 'excludes', 'needed-by', 'raised'];
const clear = () => {
  document.body.classList.remove('box-focus');
  for (const el of [...cells, ...rows]) el.classList.remove(...marks);
};
const focusOn = cell => {
  clear();
  const key = cell.dataset.key;
  const mark = (keys, cls) => { for (const c of cells) if (keys.includes(c.dataset.key)) c.classList.add(cls); };
  mark(keysOf(cell, 'needs'), 'needs');
  mark(keysOf(cell, 'needsOneOf'), 'needs-one-of');
  mark(keysOf(cell, 'excludes'), 'excludes');
  for (const c of cells) {
    if (keysOf(c, 'excludes').includes(key)) c.classList.add('excludes');
    if (keysOf(c, 'needs').includes(key) || keysOf(c, 'needsOneOf').includes(key)) c.classList.add('needed-by');
  }
  const raised = keysOf(cell, 'raises');
  for (const r of rows) if (raised.includes(r.dataset.key)) r.classList.add('raised');
  cell.classList.add('self');
  document.body.classList.add('box-focus');
};
for (const c of cells) {
  c.addEventListener('mouseenter', () => focusOn(c));
  c.addEventListener('focus', () => focusOn(c));
  c.addEventListener('mouseleave', clear);
  c.addEventListener('blur', clear);
}
