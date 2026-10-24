---
title: How devices sync
pick: one
options:
  crdt:
    title: CRDT per note
    needs: [store/sqlite]
  lww:
    title: Last write wins per file
  git:
    title: Git repo under the hood
    needs: [store/files]
---
Devices edit offline and reconcile when they reconnect.

## sync/crdt

Each note is a CRDT document; concurrent edits merge character by
character.

## sync/lww

The most recently written copy of a file replaces the others.

## sync/git

The notes folder is a git repository; sync is commit, pull and push.
