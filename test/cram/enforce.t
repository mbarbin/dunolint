The dunolint command line tool comes with a util to enforce conditions
interactively from the terminal.

Initialize the project root.

  $ touch dune-workspace

There is nothing to lint on an empty project.

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

Let's exercise cases where the sexp supplied is invalid.

  $ dunolint lint --dry-run --enforce '(blah'
  dunolint: option '--enforce': (parse_error.ml.Parse_error
              ((position ((line 1) (col 5) (offset 5)))
                (message "unclosed parentheses at end of input")))
  Usage: dunolint lint [OPTION]…
  Try 'dunolint lint --help' or 'dunolint --help' for more information.
  [124]

We hide the output of this one because it contains an unstable file path.

  $ dunolint lint --dry-run --enforce '(blah)' 2> /dev/null
  [124]

Test precedence between --config and --enforce flags.
When both are provided, --enforce should take precedence (be applied last).

First, create a config file that enforces (is_prefix main):

  $ cat > test-config <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce
  >   (dune_project
  >    (name (is_prefix main)))))
  > EOF

With only --config, it should enforce (is_prefix main):

  $ dunolint lint --dry-run --config test-config
  dry-run: Would edit file "subrepo/dune-project":
  -1,3 +1,3
  -|(name subrepo)
  +|(name mainsubrepo)
    
    (generate_opam_files)

Now with both --config and --enforce, --enforce is applied last.
Both rules apply, but --enforce's (is_prefix sub) is applied after config's (is_prefix main):

  $ dunolint lint --dry-run --config test-config --enforce '(dune_project (name (is_prefix sub)))'
  dry-run: Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name submain)
  
  dry-run: Would edit file "subrepo/dune-project":
  -1,3 +1,3
  -|(name subrepo)
  +|(name submainsubrepo)
    
    (generate_opam_files)

Test interaction between auto-loaded configs and --enforce.
Using --enforce disables autoloading to simplify precedence semantics:

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce
  >   (dune_project
  >    (name (is_prefix auto)))))
  > EOF

With auto-loaded config enforcing (is_prefix auto):

  $ dunolint lint --dry-run
  dry-run: Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name automain)
  
  dry-run: Would edit file "subrepo/dune-project":
  -1,3 +1,3
  -|(name subrepo)
  +|(name autosubrepo)
    
    (generate_opam_files)

With --enforce, autoloading is disabled so only enforce rules apply:

  $ dunolint lint --dry-run --enforce '(dune_project (name (is_prefix cmd)))'
  dry-run: Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name cmdmain)
  
  dry-run: Would edit file "subrepo/dune-project":
  -1,3 +1,3
  -|(name subrepo)
  +|(name cmdsubrepo)
    
    (generate_opam_files)
