In this test we monitor that dunolint can be run within an hg repo.

  $ hg init 2> /dev/null

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (dune_lang_version (greater_than_or_equal_to 3.19)))))
  > EOF

  $ dunolint lint --check
  check: Would edit file "dune-project":
  -1,1 +1,1
  -|(lang dune 2.0)
  +|(lang dune 3.19)
  Error: Linting check failed: Exiting with unaddressed linting errors.
  [123]

