---
title: What the merge screen compares
pick: one
raised-by: [conflict/diff]
options:
  two-way:
    title: Mine against theirs
  three-way:
    title: Mine and theirs against their common ancestor
    needs: [sync/git]
---
What the merge screen puts next to each other.

## conflict/two-way

The two conflicting versions only.

## conflict/three-way

Both versions against the last version they shared. Three-way needs a
merge base, which only git history supplies here.
