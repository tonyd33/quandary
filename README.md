# quandary

Explore design space. Write space as Markdown files: questions, their options, constraints between options. Your rulings (accept this, defeat that) recorded in named points. quandary says which options stay possible, which forced, which rulings responsible, what decide next.

Suits decisions with many interlocking parts: language feature, protocol, architecture. Objection to option = question raised by that option, whose options = ways to resolve it, so record of why something ruled out lives beside alternatives.

## Install

With nix, from clone:

```
nix run . -- --help
nix profile install .
```

With cabal (GHC 9.10), CaDiCaL SAT solver library where `pkg-config` finds it:

```
cabal install
```

## A walkthrough

`test/fixtures/notes-sync` = small space: notes app syncing between devices. Copy, look around:

```
$ cp -r test/fixtures/notes-sync /tmp/notes && cd /tmp/notes
$ quandary status
point current

frontier
  sync/how  git, other

raised if taken
  conflict/shown        by sync/git
  conflict/compare      by conflict/diff
  ...

closed
  store/where (files)

dormant
  store/editors

points
  current       fresh
  edit-offline  fresh, extends current
```

Frontier lists open questions in decide order, each with keys of options still possible at current point; `other` named when one listed option left. Question only untaken option raises waits under "raised if taken". `show` gives question in full: every option with verdict (possible, forced, impossible) or ruling on it, plus `sync/how.other`, standing for unlisted answer:

```
$ quandary show sync/how
sync/how: How devices sync [open]
pick: one, a root

Devices edit offline and reconcile when they reconnect.

impossible  sync/crdt       CRDT per note
defeated    sync/lww        Last write wins per file  silently loses edits made offline
  raises conflict/shown: What the user sees on a conflict
possible    sync/git        Git repo under the hood
  raises conflict/shown: What the user sees on a conflict
  raises sync/merge-conflicts: Merge conflicts surface to non-technical users
possible    sync/how.other
```

Question raised only with other options names them under it: `with any of a, b`.

Ask why option out:

```
$ quandary why sync/crdt
sync/crdt is impossible because of:
  accept store/files (current: must open in any editor)
```

Try ruling without recording, then record. `whatif accept` also lists what accepted option commits to: each question it raises, with possible options and their bounds at point it would reach:

```
$ quandary whatif accept sync/git
sync/how: open -> closed (git forced)

raises
  conflict/shown: What the user sees on a conflict
    copy, diff
  sync/merge-conflicts: Merge conflicts surface to non-technical users
    driver, other
$ quandary accept sync/git history and offline merges
accepted sync/git
  sync/how: open -> closed
```

Ruling no design can satisfy refused, with conflicting rulings:

```
$ quandary accept sync/crdt
quandary: refused: no design would reach the point, because of:
  accept sync/crdt
  accept sync/git (current: history and offline merges)
```

To work out question without committing to option raising it, branch: point extending `current`.

```
$ quandary point new offline-edit current
created point offline-edit extending current
$ quandary --point offline-edit accept conflict/diff
$ quandary diff current offline-edit
diff current offline-edit

                 current   offline-edit
conflict/shown   open      closed
  conflict/copy  possible  impossible
  conflict/diff  possible  accepted
```

To take branch, `quandary adopt offline-edit` moves its rulings into `current`, removes it.

`quandary check` reports every problem in files; `quandary graph` prints space as Mermaid flowchart. Rest of README = reference.

## A space

Directory holding `questions/`, `notes/`, `points/`, optionally `quandary.yaml`. Each question and note = Markdown file with YAML frontmatter, in topic subdirectories any depth. Tool reads `questions/`, `notes/`, `quandary.yaml`, never writes them; writes only `points/`.

### Ids

One namespace holds every question, option, note:

- question or note = path under `questions/` or `notes/` without `.md`: `questions/keys/where.md` is `keys/where`;
- question file named after its directory = that directory: `questions/sync/stale-copy/stale-copy.md` is `sync/stale-copy`;
- option = question file's directory + key: option `local` of `keys/where` is `keys/local`, option `merge` of `sync/stale-copy` is `sync/stale-copy/merge`;
- every question also has option `<question>.other`, something unlisted; `other` not key author writes.

No two nodes share id, ignoring case. Path segment = letters, digits, `-`, `_`. Name question for topic: origin is its `raised-by`, and which question option answers is where declared, so moving option between questions of one directory keeps id.

### Question

````markdown
---
title: Where keys live
pick: one
raised-by: [encrypt/e2e]
options:
  local:
    title: Keys stay on device
    excludes: [sync/server-merge]
  server:
    title: Keys escrowed on a server
    needs: [account/password]
    assessment: {grade: costly, why: recovery needs a password reset flow}
    links:
      - {rel: rivals, to: keys/local, for: recovery after device loss}
forbid:
  - all: [sync/git, encrypt/e2e, not keys/local]
    why: git and e2e together need local keys
links:
  - {rel: gains, to: owns-data}
---
Who holds the encryption keys decides who can read notes and whether a
lost device loses them.

## keys/local

Keys are generated on each device and never leave it. A new device pairs
with an existing one. Losing every device loses the notes.

## keys/server

Keys are escrowed, encrypted under the account password. Recovery works
after device loss; whoever holds the password can read the notes.
````

| key | on | meaning |
|---|---|---|
| `title` | question, option, note | required one-line name |
| `pick` | question | required: `one`, `some` (one or more) or `any` |
| `raised-by` | question | raisers, each option or list of options taken together; absent for root |
| `options` | question | option key to option |
| `forbid` | question | entries `{all: [literal, ...], why: text}`; `why` optional |
| `needs` | option | options all taken with this one |
| `needs-one-of` | option | options, at least one taken with this one |
| `excludes` | option | options never taken with this one |
| `assessment` | option | `{grade: g, unverified: true, why: text}`: assessment, not ruling; `grade` one of space's grades, rest optional |
| `links` | question, option, note | entries `{rel: label, to: id}`, other keys free text |

Body opens with question's description, then one section `## <option id>` per option describing it. Every description required, non-empty; other headings free prose.

Literal = option id, taken, or `not` + option id, refused. `forbid` entry may name options of any question; no two entries hold same literals. `needs`, `needs-one-of`, `excludes`, `raised-by`, `forbid` may name `<question>.other`. `forbid` entry holds in every design, whether or not its question live; to forbid only where option raises it, name that option in entry.

Question raised by several options = one question in every design: its rulings hold whichever raiser made it live. To rule per raiser, give each raiser own question.

Link labels = kebab-case words (`rivals`, `variant-of`, `same-as`, `gains`, `harms`, `needs-capability`); targets must exist. Links recorded and drawn, never reasoned with. `links` lists labels in use.

Objection to option = question raised by it, options = ways to resolve. `raised-by: [a, b]` raises question in designs taking either; `raised-by: [[a, b]]` only in designs taking both.

### Note

Anything link points at that isn't question or option: criterion, requirement, ticket. Frontmatter = `title` and `links`; body = required description.

```markdown
---
title: User can leave with their notes in a readable format
---
Readable means openable without this app, by any text editor.
```

### Configuration

`quandary.yaml` configures space:

```yaml
grades: [broken, hack, costly, clean]
```

| key | meaning |
|---|---|
| `grades` | grades option may be assessed at, lowest first; letters, digits, `-`, `_` |

Space chooses grades; tool knows only order. Assessment in space declaring no grades = problem.

### Frontmatter

Every scalar is text: `pick: no` is text `no`, never boolean. Unknown keys, duplicate keys, aliases, tags = problems. Quote value holding ` #`, which YAML otherwise reads as comment, or starting with quote it doesn't end with.

## Points

`points/<name>.md`. Point = named set of rulings; `current` = where user stands, empty when file absent. Tool writes these files; body free prose.

```yaml
---
extends: current
accept:
  store/files: {title: Plain Markdown files in a folder, reason: must open in any editor}
  sync/git: {title: Git repo under the hood}
defeat:
  sync/lww: {title: Last write wins per file, reason: silently loses offline edits}
  sync/how.other: {reason: these three are all there is}
drop: [conflict/silent]
defer: [diff/phone]
prefer: {sync/git: 8, sync/crdt: 6}
---
```

| key | meaning |
|---|---|
| `extends` | point whose rulings this one inherits, transitively |
| `proposed` | `true` when rulings are agent's proposals, not user's |
| `accept` | options taken, each with title ruled on, optional reason |
| `defeat` | options refused, each with title ruled on, reason |
| `drop` | options whose inherited ruling this point does without |
| `defer` | questions left off frontier |
| `prefer` | option to rank, integer 0 to 10, for display order |

Ruling records option's title when made; ruling whose title no longer matches = problem, settled by ruling again or restoring title. Ruling on `<question>.other` records no title. Reason, when given, is never blank.

Point's own ruling on option replaces inherited one; dropped option inherits none. Point that accepts and defeats one option, drops option it rules, or drops one no extended point rules = problem.

Branch ("if we do sequence patterns, here is how") = point extending `current` that accepts raiser and answers under it, rules differently where must. Example = point extending `current` accepting what example assumes. `adopt` takes branch into point it extends. Proposed point holds rulings agent worked out for user to confirm.

## Semantics

Design decides every option: taken or refused. Question live in design taking every option of one of its raisers, and always when root. Design valid when holding no forbidden set. Forbidden sets = `all` of each `forbid` entry, plus for every option `o` of question `Q`, `Q.other` included:

| source | forbidden set |
|---|---|
| `o excludes: [x]` | `o`, `x`, for each `x` |
| `o needs: [x]` | `o`, `not x`, for each `x` |
| `o needs-one-of: [x, y]` | `o`, `not x`, `not y` |
| `Q raised-by: [r, [s, t]]` | `o`, `not r`, `not s`; `o`, `not r`, `not t` |
| `Q pick: one` | `o`, `p` for each other option `p` of Q |
| `Q pick: one` or `some`, answered while live | for each raiser: its options and `not` every option of Q; for root: `not` every option of Q |

So options of non-live question refused, and live `pick: one` question takes exactly one option, `Q.other` when nothing listed fits. Question with no listed options takes `Q.other`, constrains nothing. Defeating `Q.other` says listed options are all there is. Live `pick: some` question takes one or more options. `pick: any` question takes any of its options, none included; its `Q.other` = something unlisted, taken alongside rest. Question at root of `questions/` names options by bare key.

Valid design reaches point when taking every option point accepts and refusing every option it defeats. At point:

- option possible when some design reaching point takes it, impossible when none does, forced when every one does;
- point stale when no design reaches it;
- `why O` lists every minimal subset of point's rulings no design taking `O` reaches; empty subset means no valid design takes `O`.

Rulings are positions, not conditions: accepting answer under question raised by `r` reaches only designs taking `r`. To work out question without committing to raiser, rule in branch.

Editing space changes designs. Edit only adding designs (new option, dropped constraint) can only weaken verdicts: nothing newly impossible or forced. Edit only removing designs (new constraint) can only strengthen them. Point stays fresh under edit only adding designs.

`defer`, `prefer`, links, notes, assessments, bodies change no design.

### Grades

Option commits to itself, to options taking it forces where point doesn't, and to options design takes in questions its commitments raise, transitively. Design's grade for option = lowest grade among commitments it takes; option's value at point = highest such grade over designs reaching point that take it. Options outside commitments still must fit in design, but grades don't count.

Each possible option has bounds `lo..hi`: value with every ungraded option, `<question>.other` included, counted at lowest grade, and at highest. Unverified grade counts toward `hi` only. Design at least `lo` exists whatever ungraded options turn out to be; none better than `hi`. Bounds that meet = one grade; grading more can't move them. Option whose `hi` below sibling's `lo` is dominated. `pick: any` question design may leave unanswered never lowers bound.

## Commands

`-C DIR` names space (default `.`), `--point NAME` the point (default `current`).

| command | does |
|---|---|
| `check` | report every problem in space and points, every stale point; exit 1 if any, else print `ok` |
| `status` | frontier in decision order, each question's possible options by preference, with bounds unless spanning every grade; then open questions with option taken; then other open questions no taken option raises, with raisers; then closed, deferred, dormant questions; then every point: fresh or stale, proposed, whether it rules anything beyond point it extends |
| `show QUESTION` | options with verdicts, rulings, grades, bounds, whether one dominated; under each option, each question it raises, once, with title, and on line below, options raising it alongside: `with a`, `with any of a, (b with c)`; none when option raises it alone |
| `why OPTION` | every set of rulings making `OPTION` impossible or forced |
| `whatif VERB ID ... [VERB ID ...]` | changes if rulings applied, with every conflicting set of rulings if they leave point stale; unless stale, then `raises`: every question accepted options raise, with title and co-raisers as in `show`, `[dormant]` when live in no design reaching new point, else options as in `status` at new point; each verb `accept`, `defeat` or `reopen` followed by ids, no reasons |
| `accept OPTION... [--] [REASON]` | accept each; refuses, writing nothing, if any would leave point stale |
| `defeat OPTION... [--] REASON` | defeat each; refuses, writing nothing, if any would leave point stale |
| `exhaust QUESTION... [--] REASON` | defeat each `QUESTION.other`: listed options all there is; refuses, writing nothing, if any would leave point stale |
| `settle QUESTION... [--] REASON` | defeat every still-possible option of each question, `.other` included: what's taken is all of it; refuses, writing nothing, at stale point or if any would leave point stale |
| `reopen ID...` | for each, drop point's own ruling on option, else drop inherited one; or drop question's deferral |
| `defer QUESTION` | leave off frontier |
| `prefer OPTION N` | rank 0 to 10, `-` to clear |
| `point new NAME [EXTENDS] [--proposed]` | create empty point |
| `point rm NAME` | remove point no other extends; never `current` |
| `adopt BRANCH [--confirmed]` | move branch's own rulings into point it extends, remove it; proposed branch needs `--confirmed` |
| `rulings` | point's rulings, inherited included, each with point it comes from |
| `links` | each link label in use, with link count |
| `diff POINT POINT` | each question whose state or options differ between two points |
| `graph` | space as Mermaid flowchart, options coloured by verdict |

Ruling command names what it did, then prints what moved: one line per question touched, its state change and new verdicts of options it didn't rule, by key (`.other` only when forced, or when question neither changed state nor holds ruled option), and points extending this one that went stale or fresh. Also says when option already forced or impossible, with one responsible set of rulings and how many more `why` lists; still is after `reopen`, with every set; or ranked while impossible. Gives old and new reason when option ruled same way again (no reason clears it), and names raiser an accept commits to that no ruling takes. Refusal names every conflicting set of rulings, and `pick: one` question two of them answer. Every command but `check` refuses space with problems, except ruling's out-of-date title. `status`, `show`, `why`, `diff` exit 1 at stale point.

Question dormant when live in no design reaching point; closed when one option forced (`pick: one`) or none possible (`pick: any`); deferred when point defers it; open otherwise. Frontier = open questions.

Order: question comes after questions of its raisers. Of questions that may come next, first = one touched by most constraints and raisings, then one in block just placed, then lower id. Questions joined by constraint form block.

`adopt` writes branch's rulings, drops, deferrals into point it extends, branch winning; points extending branch extend that point instead. Any other point extending that point which adoption leaves stale drops fewest adopted rulings clearing every conflict; output names them. Ruling such point holds that it now inherits unchanged gets removed from it.

## License

MIT.