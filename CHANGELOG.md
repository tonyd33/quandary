# Changelog

## 0.1.0 (unreleased)

### Breaking Changes

- **Blank reasons are problems**: `check` reports a `reason` that is empty or only whitespace, and a space holding one no longer loads until it is fixed.
- **`settle` refuses a stale point**: `settle` at a stale point is now refused, writing nothing. Previously it reported the question settled without defeating anything.

### New Features

- `graph --format dot` prints the space as a Graphviz digraph, each question a cluster holding its options. `graph` alone still prints Mermaid.
- Added `browse`, which shows a point in the terminal: its questions grouped as in `status`, the selected question as in `show` with its description and option bodies rendered as GitHub-flavoured Markdown, the questions each option raises, a page for each option, other points, and `whatif`. `g` goes to any question or option id, matched fuzzily as you type. It redraws when the space's files change.
- Added `serve [--port N]`, which serves the same pages as web pages on 127.0.0.1, every option linked to its page and a go-to box on each. A graph page draws the space as questions holding their options, coloured by verdict, each opening its page on a click. Terms such as the status groups, verdicts and bounds say what they mean on hover. A page reloads itself when the space's files change. The server refuses requests whose `Host` is not `127.0.0.1:N` or `localhost:N`, and drops raw HTML from bodies.

### Improvements

- `show` now takes an option id and prints the option's question, its ruling, every set of rulings making it forced or impossible, its bounds, rank, grade and dominator, its description and the questions it raises. It previously refused with `is an option, not a question`.
- `show` now prints each question an option raises once, with its title. Options that raise it together with that option go on a `with any of` line under it.
- `whatif accept` now lists every question the accepted options raise, with the bounds of its possible options at the point the acceptance reaches.

### Bug Fixes

- Fixed ruling commands reading an id with capital letters, such as `Naming/N3`, as the start of the reason when it followed valid ids. It is now reported as an unknown id.
- Fixed `status` listing an open question under both "taken, more possible" and "raised if taken" when one of its options was taken but none of its raisers was. It is now listed under "taken, more possible" only.
- Fixed `whatif defeat` naming its own ruling with an empty reason, `(current: )`. It now prints `(current)`.
