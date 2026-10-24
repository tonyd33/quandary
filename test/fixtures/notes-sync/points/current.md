---
accept:
  store/files: {title: Plain Markdown files in a folder, reason: must open in any editor}
defeat:
  sync/lww: {title: Last write wins per file, reason: silently loses edits made offline}
  conflict/silent: {title: Drop the loser silently, reason: data loss}
---
