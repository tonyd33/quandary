# Changelog

## 0.1.0 (unreleased)

### Breaking Changes

- **Blank reasons are problems**: `check` reports a `reason` that is empty or only whitespace, and a space holding one no longer loads until it is fixed.
- **A space holds `quandary.yaml`**: every command but `init` refuses a directory without `quandary.yaml`, naming `quandary init`. Previously any directory, even a missing one, read as a space. Run `quandary init` in an existing space to add it.
- **`settle` refuses a stale point**: `settle` at a stale point is now refused, writing nothing. Previously it reported the question settled without defeating anything.

### New Features

- Added `next [N]`. For each of the first `N` most preferred root options whose bounds can still move, it names the fewest ungraded options to grade: those that, graded above the option's lower bound, raise it, or those that, graded below its upper bound, lower it, whichever is fewer.
- Added `lock REASON` and `unlock`, marking a space mid-edit. While the lock is live, every command writing points refuses with its reason and age unless given `--force`, read-only commands warn, and `browse` and `serve` show it above every page; `serve`'s record form refuses unless its override is checked. `unlock` runs `check` before releasing, and lists every ruling forced over the lock. The lock lapses 15 minutes after the later of its taking and the last edit to the space's questions, notes or `quandary.yaml`; until someone unlocks, commands and pages warn that the edit may be unfinished.
- Added a `desc` and a `color` for each grade: an entry of `grades` in `quandary.yaml` can be a mapping of `name`, `desc` and `color`. `serve` shows a grade's `desc` on hover over its name, tints the name with its `color`, and stripes each box cell of an option assessed at it. A bare name still works.
- Added search to `serve`, linked from every page, and to `browse` under `/`: questions and options whose ids or titles match the text as go-to does, then those whose descriptions, bodies or ruling reasons hold it, each with the line that matched.
- Added a rulings page to `serve`, linked from every page: the point's accepted and defeated options, inherited ones included, each with its title, reason and the point it comes from, and a button to try reopening it.
- Added `init`, which creates a space: its directory, `questions/`, `notes/`, `points/`, a `quandary.yaml` showing grades in a comment, and `.quandary/.gitignore`. It creates only what is missing and overwrites nothing.
- `graph --format dot` prints the space as a Graphviz digraph, each question a cluster holding its options. `graph` alone still prints Mermaid.
- Added `browse`, which shows a point in the terminal: its questions grouped as in `status`, the selected question as in `show` with its description and option bodies rendered as GitHub-flavoured Markdown, the questions each option raises, a page for each option, other points, and `whatif`. `g` goes to any question or option id, matched fuzzily as you type. It redraws when the space's files change.
- Added `serve [--bind ADDR] [--port N]`, which serves the same pages as web pages on 127.0.0.1, or on `ADDR` for another device on the local network, every option linked to its page, and a go-to box on every page. A box page lists each question as a row of its options, coloured by verdict, and marks what an option needs, excludes and raises when hovered. A graph page draws the space as questions holding their options, coloured by verdict, joined by the questions they raise, each opening its page on a click; an option's needs and excludes appear when it is hovered, or all at once. Both leave out dormant questions and impossible options unless asked. Playing the box supposes accepts and defeats with clicks, outlines what the last one moved, names the rulings that conflict, and undoes; `record` writes them as rulings with a reason for each, refusing when the space changed since the page was drawn. An option page draws, under each set of rulings making the option forced or impossible, a minimal set of the space's constraints that with those rulings gives the verdict, and lists them as sentences. Terms such as the status groups, verdicts and bounds say what they mean on hover. A page reloads itself when the space's files change. The server refuses requests whose `Host` is not `localhost:N` or an IP address with `:N`, and drops raw HTML and script-running links from bodies.

### Improvements

- `check` now lists the keys a mapping accepts beside an unknown key, and for a key left by an unquoted comma inside `{}`, such as `why: cheap, no machinery`, says to quote the value.
- `show` now takes an option id and prints the option's question, its ruling, every set of rulings making it forced or impossible, its bounds, rank, grade and dominator, its description and the questions it raises. It previously refused with `is an option, not a question`.
- `show` now prints each question an option raises once, with its title. Options that raise it together with that option go on a `with any of` line under it.
- `whatif accept` now lists every question the accepted options raise, with the bounds of its possible options at the point the acceptance reaches.
- `why` and `show OPTION` now list, under each set of rulings making the option forced or impossible, a minimal set of the space's constraints that with those rulings gives the verdict.
- Ruling commands now wait for one another on `.quandary/rulings.lock`, saying so after a second, and replace point files whole. Two rulings at once both land, and a reader never sees half a file. A `.quandary/` they create holds a `.gitignore` keeping it out of version control.
- Commands load a space and answer queries faster: on a space of 250 question files, `show` and `why` take about 40% less time and `status` about 75% less, now that it bounds options on every core.

### Bug Fixes

- Fixed the graph page losing its zoom and expanded questions whenever the space changed. It now redraws in place, keeping both.
- Fixed an empty or comment-only `quandary.yaml` being reported as `expected a mapping`. It now configures nothing.
- Fixed ruling commands reading an id with capital letters, such as `Naming/N3`, as the start of the reason when it followed valid ids. It is now reported as an unknown id.
- Fixed `status` listing an open question under both "taken, more possible" and "raised if taken" when one of its options was taken but none of its raisers was. It is now listed under "taken, more possible" only.
- Fixed `whatif defeat` naming its own ruling with an empty reason, `(current: )`. It now prints `(current)`.
