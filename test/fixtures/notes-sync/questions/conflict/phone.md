---
title: Merge screen on a phone
pick: one
raised-by: [conflict/phone-layout]
options:
  stacked:
    title: Versions stacked vertically, hunks interleaved
  swipe:
    title: One version at a time, swipe to switch
  desktop-only:
    title: Phone shows the banner only, resolve on a desktop
    excludes: [conflict/modal]
---
How the merge screen fits a phone.

## conflict/stacked

Hunks from both versions alternate down the screen.

## conflict/swipe

One full version on screen; a swipe shows the other.

## conflict/desktop-only

The phone marks the note as conflicted and defers to a desktop.
