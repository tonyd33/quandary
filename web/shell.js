const palette = document.getElementById("palette");
const trigger = document.querySelector("a.find");
const input = palette.querySelector("input");
const results = document.getElementById("palette-results");
const searchLink = document.getElementById("palette-search");
let selected = 0;
let pending = null;
let timer = null;

function openPalette(event) {
  event.preventDefault();
  if (!palette.open) palette.showModal();
  input.select();
  find();
}

async function find() {
  const q = input.value.trim();
  const search = new URL(searchLink.href);
  search.searchParams.set("q", q);
  searchLink.href = search;
  pending?.abort();
  pending = new AbortController();
  if (!q) return show("");
  try {
    const response = await fetch(trigger.dataset.find + "?q=" + encodeURIComponent(q), { signal: pending.signal });
    if (response.ok) show(await response.text());
  } catch (e) {
    if (e.name !== "AbortError") throw e;
  }
}

function show(html) {
  results.innerHTML = html;
  selected = 0;
  move(0);
}

function move(by) {
  const items = results.querySelectorAll("li");
  if (!items.length) return;
  selected = (selected + by + items.length) % items.length;
  items.forEach((item, i) => item.classList.toggle("on", i === selected));
  items[selected].scrollIntoView({ block: "nearest" });
}

trigger.addEventListener("click", openPalette);
document.addEventListener("keydown", (event) => {
  if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "k") openPalette(event);
});
input.addEventListener("input", () => {
  clearTimeout(timer);
  timer = setTimeout(find, 80);
});
input.addEventListener("keydown", (event) => {
  if (event.key === "ArrowDown") {
    event.preventDefault();
    move(1);
  } else if (event.key === "ArrowUp") {
    event.preventDefault();
    move(-1);
  } else if (event.key === "Enter") {
    const chosen = results.querySelectorAll("li a")[selected];
    if (chosen) {
      event.preventDefault();
      location.href = chosen.href;
    }
  }
});
palette.addEventListener("click", (event) => {
  if (event.target === palette) palette.close();
});

function inspect(row) {
  document.querySelectorAll(".inspect").forEach((p) => (p.hidden = p.dataset.option !== row.dataset.option));
  document.querySelectorAll("tr.row").forEach((r) => r.classList.toggle("selected", r === row));
}

document.querySelectorAll("tr.row[tabindex]").forEach((row) => {
  row.addEventListener("click", (event) => {
    if (!event.target.closest("a, button, form, details")) inspect(row);
  });
  row.addEventListener("keydown", (event) => {
    if (event.key === "Enter" && event.target === row) inspect(row);
  });
});
