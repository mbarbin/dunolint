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

A [backend] condition targets the field with that backend, or adds one if there is none.
Other fields are left untouched.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (backend bisect_ppx --x)))'
  dry-run: Would edit file "dune":
  @@ -1,5 +1,5 @@
    (library
     (name mylib)
  -| (instrumentation (backend bisect_ppx))
  +| (instrumentation (backend bisect_ppx --x))
     (instrumentation (backend landmarks))
     (preprocess no_preprocessing))

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (backend other)))'
  dry-run: Would edit file "dune":
  @@ -1,5 +1,6 @@
    (library
     (name mylib)
     (instrumentation (backend bisect_ppx))
     (instrumentation (backend landmarks))
  +| (instrumentation (backend other))
     (preprocess no_preprocessing))

The instrumentation fields are a collection: a negated [backend] holds when no field has
that backend. It is not auto-fixed, and the failure is located at the stanza.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (not (backend landmarks))))'
  File "dune", lines 1-5, characters 0-133:
  1 | (library
  2 |  (name mylib)
  3 |  (instrumentation (backend bisect_ppx))
  4 |  (instrumentation (backend landmarks))
  5 |  (preprocess no_preprocessing))
  Error: Enforce Failure.
  The following condition does not hold: (not (backend landmarks))
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]

When a condition cannot be enforced, the failure is reported once and the stanza is left
unchanged.

  $ dunolint lint --dry-run --enforce '(dune (instrumentation (and (backend other) (not (backend other)))))'
  File "dune", lines 1-5, characters 0-133:
  1 | (library
  2 |  (name mylib)
  3 |  (instrumentation (backend bisect_ppx))
  4 |  (instrumentation (backend landmarks))
  5 |  (preprocess no_preprocessing))
  Error: Enforce Failure.
  The following condition does not hold:
    (and (backend other) (not (backend other)))
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]
