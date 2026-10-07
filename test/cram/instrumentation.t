Linting stanzas with several instrumentation fields.

  $ touch dune-workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (instrumentation (backend bisect_ppx))
  >  (instrumentation (backend landmarks))
  >  (preprocess no_preprocessing))
  > EOF

The file is formatted first, so that the edits below are shown on a formatted file.

  $ dunolint tools lint-file dune --in-place

  $ cat dune
  (library
   (name mylib)
   (instrumentation
    (backend bisect_ppx))
   (instrumentation
    (backend landmarks))
   (preprocess no_preprocessing))

A [backend] condition targets the field with that backend, or adds one if there is none.
Other fields are left untouched.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (backend bisect_ppx --x)))'
  dry-run: Would edit file "dune":
  @@ -1,7 +1,7 @@
    (library
     (name mylib)
     (instrumentation
  -|  (backend bisect_ppx))
  +|  (backend bisect_ppx --x))
     (instrumentation
      (backend landmarks))
     (preprocess no_preprocessing))

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (backend other)))'
  dry-run: Would edit file "dune":
  @@ -1,7 +1,9 @@
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
      (backend landmarks))
  +| (instrumentation
  +|  (backend other))
     (preprocess no_preprocessing))

A [backend] condition without flags holds when there is a field with that backend, so its
negation is the same as [absent], and is enforced by removing the field.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (not (backend landmarks))))'
  dry-run: Would edit file "dune":
  @@ -1,7 +1,5 @@
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
  -| (instrumentation
  -|  (backend landmarks))
     (preprocess no_preprocessing))

When a condition cannot be enforced, the failure is reported once, located at the stanza,
and the stanza is left unchanged.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (and (backend other) (not (backend other)))))'
  File "dune", lines 1-7, characters 0-137:
  1 | (library
  2 |  (name mylib)
  3 |  (instrumentation
  4 |   (backend bisect_ppx))
  5 |  (instrumentation
  6 |   (backend landmarks))
  7 |  (preprocess no_preprocessing))
  Error: Enforce Failure.
  The following condition does not hold:
    (and (backend other) (not (backend other)))
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]

Migrating from a backend to another is done with [present] and [absent].

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (and (present other) (absent landmarks))))'
  dry-run: Would edit file "dune":
  @@ -1,7 +1,7 @@
    (library
     (name mylib)
     (instrumentation
      (backend bisect_ppx))
     (instrumentation
  -|  (backend landmarks))
  +|  (backend other))
     (preprocess no_preprocessing))
