Test that path predicates in rules are evaluated relative to config location.

When a config at lib/dunolint contains a rule with (path (glob alpha/**)),
the glob should be evaluated relative to lib/, not the workspace root.
This ensures that hierarchical configs can use path patterns that are
meaningful relative to their own location in the directory tree.

Setup workspace:

  $ touch dune-workspace
  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (name test_project)
  > EOF

Create lib/ directory with a config:

  $ mkdir -p lib
  $ cat > lib/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   ((path (glob alpha/**)) (enforce (dune (library (name (is_prefix alpha_))))))))
  > EOF

Create lib/alpha/ and lib/beta/ directories:

  $ mkdir -p lib/alpha lib/beta lib/alpha/nested
  $ cat > lib/alpha/dune << EOF
  > (library
  >  (name alpha_lib))
  > EOF

  $ cat > lib/beta/dune << EOF
  > (library
  >  (name beta_lib))
  > EOF

  $ cat > lib/alpha/nested/dune << EOF
  > (library
  >  (name nested_lib))
  > EOF

The rule should apply to files matching alpha/** (relative to lib/):
- lib/alpha/dune should match (path alpha/dune) - already has alpha_ prefix
- lib/alpha/nested/dune should match (path alpha/nested/dune) - needs alpha_ prefix
- lib/beta/dune should NOT match (path beta/dune)

Run lint:

  $ dunolint lint --dry-run
  dry-run: Would edit file "lib/alpha/nested/dune":
  -1,2 +1,2
    (library
  !| (name alpha_nested_lib))
