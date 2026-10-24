---
title: What the user sees on a conflict
pick: one
raised-by: [sync/lww, sync/git]
options:
  copy:
    title: Keep both as "note (conflict).md"
    excludes: [store/sqlite]
  diff:
    title: Side-by-side merge screen
  silent:
    title: Drop the loser silently
---
Two devices changed the same note while apart.

## conflict/copy

The losing version is saved beside the note under a conflict name.

## conflict/diff

A screen shows both versions and lets the user pick per hunk.

## conflict/silent

One version is kept and the other discarded without notice.
