The dunolint command line tool comes with a util to enforce conditions
interactively from the terminal.

  $ dunolint lint

Let's add some files.

  $ cat > dune-project <<EOF
  > (name main)
  > EOF

  $ mkdir subrepo

  $ cat > subrepo/dune-project <<EOF
  > (name subrepo)
  > 
  > (generate_opam_files)
  > EOF

  $ touch dune subrepo/dune

  $ dunolint lint --dry-run --enforce false
  File "dune-project", line 1, characters 0-11:
  1 | (name main)
      ^^^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: false
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  
  File "subrepo/dune-project", line 1, characters 0-14:
  1 | (name subrepo)
      ^^^^^^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: false
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  
  File "subrepo/dune-project", line 3, characters 0-21:
  3 | (generate_opam_files)
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: false
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]

  $ dunolint lint --dry-run --enforce '(dune_project (name (is_prefix sub)))'
  dry-run: Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name submain)

  $ dunolint lint --dry-run --enforce '(dune_project (name (not (is_prefix sub))))'
  dry-run: Would edit file "subrepo/dune-project":
  -1,3 +1,3
  -|(name subrepo)
  +|(name repo)
    
    (generate_opam_files)

  $ dunolint lint --dry-run --enforce '(dune_project (name (not (equals main))))'
  File "dune-project", line 1, characters 0-11:
  1 | (name main)
      ^^^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: (not (equals main))
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]
