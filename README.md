# Screamer 4.0.1

Screamer is an extension of Common Lisp that adds support for
nondeterministic programming. It consists of two layers:

- **Nondeterministic substrate** — backtracking and undoable side effects
- **Constraint programming** — formulate and solve mixed numeric and
  symbolic constraint systems on top of the substrate

Together these augment Common Lisp with practically all of the
functionality of Prolog and constraint logic programming languages such
as CHiP and CLP(R), while interoperating with the rest of Common Lisp.

Screamer is efficient: code is transformed to plain Common Lisp at
compile time, deterministic portions pass through unchanged, and only
the nondeterministic regions pay any backtracking overhead.

Originally written by Jeffrey Mark Siskind and David Allen McAllester.
See `LICENSE` for terms.

## Status

This is **Screamer 4.0.1**, the active maintenance branch. It builds
on the 3.20-derived modernisation effort by Nikodemus Siivola, adopts
several portable improvements from Swapneil Singh's
[Screamer 5](https://github.com/swapneils/screamer) (credited inline
below), and adds support for rationals, ratios, floats, dependency
tracking, and a walker that handles `MACROLET`, `SYMBOL-MACROLET`,
`LOAD-TIME-VALUE`, and `LOCALLY` inside nondeterministic contexts.

Tested on SBCL, CCL, LispWorks, and Allegro. Depends on
[Alexandria](https://common-lisp.net/project/alexandria/).

## Public API additions

### Search and value collection

- `n-values` — collect the first N nondeterministic values; analogous
  to `all-values` but bounded. Adapted from Kilian Sprotte's
  [SMC(PWGL)](https://github.com/openmusic-project/PWGL) (2007).
- `mapcar-nondeterministic` — `cl:mapcar` lifted for nondeterministic
  functions. Adopted verbatim from
  [Screamer 5](https://github.com/swapneils/screamer)
  (Swapneil Singh).
- `map-nondeterministic` — `cl:map` lifted for nondeterministic
  functions.
- `lambda-nondeterministic` — anonymous nondeterministic function-
  object (CPS-converted at macroexpand time, captures lexical
  environment, must be invoked via `funcall-nondeterministic`).

### Numeric types

Rational, ratio and float variables and generators, none of which
exist in Screamer 5.

- Rational generators: `a-rational`, `a-rational-above`,
  `a-rational-below`, `a-rational-between` (Farey-walking, terminates
  with bounds).
- Ratio generators (rationals excluding integers): `a-ratio`,
  `a-ratio-above`, `a-ratio-below`, `a-ratio-between`.
- Float variables: `a-floatv`, `a-float-abovev`, `a-float-belowv`,
  `a-float-betweenv`.
- Rational and ratio variables: `a-rationalv`, `a-rationalv-*v`,
  `a-ratiov`, `a-ratiov-*v`.
- Type predicates lifted: `rationalpv`, `ratiopv`, `floatpv`.

### Search control

- `random-force` — force a variable by picking randomly from its
  domain (bounded by `*maximum-random-domain-size*`).
- `a-random-member-of` / `a-random-member-ofv` — visit elements in
  random permutation.
- `n-variables`, `n-lists-of-variables` — bulk variable construction.
- `optimize-value` — generalised `min`/`max` optimisation that prunes
  partial states early via constraint noticers (replaces
  `best-value`).

### Constraint primitives

- `all-differentv` — pairwise inequality across a variable list,
  returns a boolean variable. Implemented with the classic
  `known?-all-differentv` / `assert!-all-differentv` dispatch.
- `divide-and-conquer-force` extended to rationals and ratios with
  `closest-ratio-upper` / `closest-ratio-lower` bisection that
  respects `variable-max-denom`.
- `with-trail` — bind a fresh `*trail*` of a given size for
  pre-allocation in long searches. Adopted verbatim from Screamer 5
  (Swapneil Singh).
- Dependency tracking: `attach-noticer!` accepts `:dependencies` (the
  keyword and its `attach-dependencies!-internal` helper are
  adopted verbatim from Screamer 5). The ordering primitive that
  walks dependencies — `static-ordering-internal` — was rewritten
  here to recurse into a variable's dependencies inline before
  forcing it.

### Conditions and errors

- `screamer-error` condition class (with `message` and `args` slots).
  Adopted verbatim from Screamer 5 (Swapneil Singh).

### Output

- `local-output` — captures `*standard-output*` and sends the text to
  the *Screamer Output* buffer in Emacs through a swank channel; the
  text is removed when the enclosing choice unwinds.

## Internal changes

These are not part of the public API but they are visible to readers
of the source.

### Walker and CPS

- `MACROLET`, `SYMBOL-MACROLET`, and `LOCALLY` supported in
  nondeterministic context. The walker dispatches them and the CPS
  converter recurses through with environment augmentation. Screamer 5
  has experimental MACROLET / SYMBOL-MACROLET support via runtime
  `EVAL`; ours preserves the wrappers in the CPS-converted code
  instead.
- `LOAD-TIME-VALUE` recognised by the walker.
- LABELS-bound continuation closures declared `dynamic-extent` in
  `cps-convert-tagbody` for stack allocation.
- TYPES parameter propagated through `cps-convert-call` and
  `cps-convert-multiple-value-call-internal`: magic continuations
  receive a `(the (and …) …)` wrap via
  `inject-type-into-magic-continuation`; literal `#'foo` or
  uninterned-symbol continuations route through
  `wrap-runtime-the-continuation`.

### Runtime

- `copy-output-value` — typecase that deep-copies cons (`copy-tree`),
  hash-table (`alexandria:copy-hash-table` + recurse), sequence
  (`copy-seq` + recurse) and structure-object (`copy-structure`).
  Adopted from Screamer 5 5.1.3 (Swapneil Singh) with a small
  adjustment (iterates the source hash-table, not the copy, while
  writing the new values). `all-values` and `n-values` apply it
  unconditionally to every collected value to prevent trail-unwind
  from corrupting an already-collected result.
- `variable-dependencies` slot on the `variable` defstruct. Adopted
  verbatim from Screamer 5 (Swapneil Singh).
- `domain-size` integer branch returns
  `(1+ (- (floor upper) (ceiling lower)))`. Inspired by the
  Screamer 5 5.2.2 fix `(1+ (floor (- upper lower)))`, refined here
  so that non-integer-bound ranges such as `[1/2, 7/2]` return the
  correct count.
- Arithmetic propagation rules: new `--rule-up` and `/-rule-up`
  symmetric to `+-rule-up` and `*-rule-up`; float-to-nonrational
  guard in `+-rule-down` and `*-rule-down` tightened from
  `(or float-z float-y)` to `(and float-z rational-y)`. New
  numeric-type helpers `numeric-type-tag`, `contagious-zero`,
  `identity-shortcut-safe?` used by the refactored `+v2`, `-v2`,
  `*v2`, `/v2`; the noticer-attach path of `*v2`/`/v2` was extracted
  into `general-*v2`/`general-/v2`. `/v2` cond order corrected so
  that `(zerop y) -> (fail)` precedes `(zerop x) -> 0`.
- `peal-off-documentation-string-and-declarations` delegates to
  `alexandria:parse-body` so declare-before-doc and mixed orderings
  extract the docstring correctly.
- `define-condition-compile-time` macro mirrors the existing
  compile-time infrastructure. Adopted verbatim from Screamer 5
  (Swapneil Singh).
- `defparameter-compile-time` macro, same pattern as above.

## Standard preamble

The default user package `:screamer-user` is created on load. For small
programs:

```lisp
(in-package :screamer-user)
```

For larger programs, define your own Screamer-aware package:

```lisp
(in-package :cl-user)

(screamer:define-screamer-package :my-package
  ...optional defpackage options...)

(in-package :my-package)
```

`define-screamer-package` shadow-imports `defun`, `multiple-value-bind`,
and `y-or-n-p` from `:screamer` so nondeterministic semantics are
available without manual shadowing.

## Companion files

- **`screams.lisp`** — examples from the Screamer manual and the
  `ircs-93-03` and `aaai93` papers. Load Screamer + Iterate first, then
  this file, then `(in-package :screams)`.
- **`equations.lisp`** — equations for testing the numeric constraint
  solver. Load Screamer + this file, then `(in-package :screams)`.
- **`screamer-slime.el`** — Emacs Lisp integration for SLIME. Provides
  the Emacs-side hooks for `local-output` (live-observation text in
  the *Screamer Output* buffer that is automatically deleted on
  backtrack). To use: add this file's directory to `load-path`,
  `(require 'screamer-slime)` from your Emacs init, then on the Lisp
  side set `screamer:*iscream?*` to `T` after Screamer loads.

## Papers

The `papers/` subdirectory contains the historical Screamer manual and
the foundational papers. The code in those documents matches the
2.4-era API; equivalent updated examples live in `screams.lisp`.

- `screamer.{pdf,dvi,ps}` — the original (outdated) manual
- `ircs-93-03.{pdf,dvi,ps}` — Siskind & McAllester (1993),
  *Screamer: A Portable Efficient Implementation of Nondeterministic
  Common Lisp*. IRCS Technical Report 93-03.
- `aaai93.{pdf,dvi,ps}` — Siskind & McAllester (1993),
  *Nondeterministic Lisp as a Substrate for Constraint Logic Programming*.
  AAAI-93.

## TODO

See [TODO](TODO) for the active task list. Recent activity covers walker
cleanup, gensym renaming by role, public docstring normalisation, and
the next planned items (multi-store LOCAL SETF, SCREAMER:INCF/PUSH/POP
for trail-safe destructive ops on defstruct slots, SLIME port of
`local-output`).
