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

This is **Screamer 4.0.1**, the active maintenance branch. It builds on
the 3.20-derived modernisation effort by Nikodemus Siivola and adds
support for rationals, ratios, floats, dependency tracking, FLET/LABELS
in nondeterministic contexts, and a new walker that handles `LOOP`,
`MACROLET`, `SYMBOL-MACROLET`, and `LOAD-TIME-VALUE` correctly.

Tested on SBCL, CCL, LispWorks, and Allegro. Depends on
[Alexandria](https://common-lisp.net/project/alexandria/).

## What's new in 4.0.1

### Search and value collection

- `n-values` — collect the first N nondeterministic values; analogous to
  `all-values` but bounded
- `mapcar-nondeterministic` / `map-nondeterministic` — `cl:mapcar` /
  `cl:map` lifted for nondeterministic functions
- `lambda-nondeterministic` — anonymous nondeterministic function-object
  (CPS-converted at macroexpand time, captures lexical environment, must
  be invoked via `funcall-nondeterministic`)
- `optimize-value` — generalised `min`/`max` optimisation that prunes
  partial states early via constraint noticers (replaces `best-value`)

### Numeric types

- Rational generators: `a-rational`, `a-rational-above`, `a-rational-below`,
  `a-rational-between` (Farey-walking, terminates with bounds)
- Ratio generators (rationals excluding integers): `a-ratio`,
  `a-ratio-above`, `a-ratio-below`, `a-ratio-between`
- Float variables: `a-floatv`, `a-float-abovev`, `a-float-belowv`,
  `a-float-betweenv`
- Rational/ratio variables: `a-rationalv`, `a-rationalv-*v`, `a-ratiov`,
  `a-ratiov-*v`
- Type predicates lifted: `rationalpv`, `ratiopv`, `floatpv`

### Search control

- `random-force` — force a variable by picking randomly from its domain
  (bounded by `*maximum-random-domain-size*`)
- `a-random-member-of` / `a-random-member-ofv` — visit elements in random
  permutation
- `n-variables`, `n-lists-of-variables` — bulk variable construction

### Constraint primitives

- `all-differentv` — pairwise inequality across a variable list, returns
  a boolean variable
- Dependency tracking: `attach-noticer!` accepts `:dependencies`; ordering
  primitives (`reorder`, `static-ordering-internal`) walk dependencies
  before forcing
- `divide-and-conquer-force` extended to rationals and ratios

### Walker and CPS

- New `LOOP` walker: deterministic LOOPs go through host macroexpansion;
  nondeterministic LOOPs are rewritten as labels-based recursion (CPS
  conversion of LOOP's internal SETQs is unsound)
- `MACROLET` and `SYMBOL-MACROLET` supported in nondeterministic context
- `FLET` and `LABELS` supported via `cps-convert-flet/labels`
- Continuation closures stack-allocated via FLET aliasing in
  `possibly-beta-reduce-funcall`

### Stricter propagation

- `*minimum-shrink-ratio*` defaults to `1e-8` (was `1e-2`); smaller
  domain reductions still trigger noticers

### Conditions and errors

- New `screamer-error` condition class with `message` and `args` slots
- `define-condition-compile-time` and `defparameter-compile-time` macros
  to mirror the existing compile-time infrastructure

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
