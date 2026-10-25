// Hovering or focusing an option marks the options it needs, needs one
// of and excludes, the options needing or excluding it, and the
// questions it raises; the rest dims.
const keysOf = (el, name) => (el.dataset[name] || '').split(' ').filter(Boolean);
const cells = new Map([...document.querySelectorAll('.cell')].map(c => [c.dataset.key, c]));
const rows = new Map([...document.querySelectorAll('.box-row')].map(r => [r.dataset.key, r]));
const pointing = new Map();
const pointAt = (to, cls, from) => {
  if (!pointing.has(to)) pointing.set(to, []);
  pointing.get(to).push([cls, from]);
};
for (const [key, cell] of cells) {
  for (const k of keysOf(cell, 'needs')) pointAt(k, 'needed-by', key);
  for (const k of keysOf(cell, 'needsOneOf')) pointAt(k, 'needed-by', key);
  for (const k of keysOf(cell, 'excludes')) pointAt(k, 'excludes', key);
}
let marked = [];
const mark = (el, cls) => {
  if (!el) return;
  el.classList.add(cls);
  marked.push([el, cls]);
};
const clear = () => {
  document.body.classList.remove('box-focus');
  for (const [el, cls] of marked) el.classList.remove(cls);
  marked = [];
};
const focusOn = cell => {
  clear();
  for (const k of keysOf(cell, 'needs')) mark(cells.get(k), 'needs');
  for (const k of keysOf(cell, 'needsOneOf')) mark(cells.get(k), 'needs-one-of');
  for (const k of keysOf(cell, 'excludes')) mark(cells.get(k), 'excludes');
  for (const [cls, from] of pointing.get(cell.dataset.key) || []) mark(cells.get(from), cls);
  for (const k of keysOf(cell, 'raises')) mark(rows.get(k), 'raised');
  mark(cell, 'self');
  document.body.classList.add('box-focus');
};
for (const cell of cells.values()) {
  cell.addEventListener('mouseenter', () => focusOn(cell));
  cell.addEventListener('focus', () => focusOn(cell));
  cell.addEventListener('mouseleave', clear);
  cell.addEventListener('blur', clear);
}
