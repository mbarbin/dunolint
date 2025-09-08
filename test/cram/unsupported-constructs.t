Sometimes dunolint fails to parse a dune construct, due to some programming
error, or dunolint not being up to date with dune. In this test we monitor the
errors produced in such cases.

  $ cat > dune-project <<EOF
  > (name main)
  > 
  > (implicit_transitive_deps unknown)
  > EOF

  $ mkdir -p lib/foo

  $ cat > lib/foo/dune <<EOF
  > (library
  >  (name bar)
  >  (modes unknown))
  > EOF

  $ dunolint lint
  File "dune-project", line 3, characters 26-33:
  3 | (implicit_transitive_deps unknown)
                                ^^^^^^^
  Error: Unsupported implicit_transitive_deps value [unknown].
  
  File "lib/foo/dune", line 3, characters 8-15:
  3 |  (modes unknown))
              ^^^^^^^
  Error: compilation_mode.T: no matching variant found
  [123]
