Test the if_present construct for conditional field enforcement.

Initialize the project root.

  $ touch dune-workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (name test)
  > EOF

Create two libraries: one with public_name, one without.

  $ mkdir with-public-name without-public-name

  $ cat > with-public-name/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name my-public-lib))
  > EOF

  $ cat > without-public-name/dune <<EOF
  > (library
  >  (name mylib))
  > EOF

With [if_present]: the constraint is applied to libraries that have a public_name,
and gracefully skipped for libraries without one (no error, no change).

  $ dunolint lint --dry-run --enforce '(dune (library (if_present (public_name (is_prefix "lib.")))))'
  dry-run: Would edit file "with-public-name/dune":
  -1,3 +1,3
    (library
     (name mylib)
  !| (public_name lib.my-public-lib))

Without [if_present]: the constraint is applied to libraries with a public_name,
but enforcement fails for libraries without one (is_prefix cannot initialize
an absent field).

  $ dunolint lint --dry-run --enforce '(dune (library (public_name (is_prefix "lib."))))'
  File "without-public-name/dune", lines 1-2, characters 0-23:
  1 | (library
  2 |  (name mylib))
  Error: Enforce Failure.
  The following condition does not hold: (public_name (is_prefix lib.))
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  
  dry-run: Would edit file "with-public-name/dune":
  -1,3 +1,3
    (library
     (name mylib)
  !| (public_name lib.my-public-lib))
  [123]
