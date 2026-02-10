We exercise here the behavior of the linter upon encountering invalid files.

Initialize the project root.

  $ touch dune-workspace

Create some invalid files.

  $ cat > dune-project <<EOF
  > (name main))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name bar)
  > EOF

Create one valid file with a lint.

  $ mkdir foo
  $ cat > foo/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name bar)
  > EOF

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > EOF

Below we monitor that the invalid files are reported, but that doesn't prevent
other lint checks to be performed on the rest of the files that are valid.

  $ dunolint lint --check
  File "dune", line 3, characters 0-0:
  Error: unclosed parentheses at end of input
  
  File "dune-project", line 1, characters 11-11:
  1 | (name main))
                 
  Error: unexpected character: ')'
  
  check: Would edit file "foo/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name bar)
  +|(name foo)
  
  Error: Linting check failed: Exiting with unaddressed linting errors.
  [123]
