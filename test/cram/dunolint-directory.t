Test that a directory named `dunolint` doesn't create a config discovery issue.

  $ touch dune-workspace

  $ mkdir -p dunolint

  $ cat > dunolint/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals dunolint)))))
  > EOF

  $ cat > dunolint/dune-project << EOF
  > (lang dune 3.18)
  > 
  > (name test)
  > EOF

  $ dunolint lint
  Editing file "dunolint/dune-project":
  -1,3 +1,3
    (lang dune 3.18)
    
  -|(name test)
  +|(name dunolint)

