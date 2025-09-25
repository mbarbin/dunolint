In this test we monitor certain error messages when loading invalid build files.
In particular we look for certain regression to make sure errors are located and
messages are clear.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ dunolint lint

  $ cat > dune-project <<EOF
  > (lang dune BLAH.4)
  > EOF

  $ dunolint lint
  File "dune-project", line 1, characters 11-17:
  1 | (lang dune BLAH.4)
                 ^^^^^^
  Error: Invalid version format: "BLAH.4".
  [123]

  $ cat > dune-project <<EOF
  > (lang dune 3.18.2)
  > EOF

  $ dunolint lint
  File "dune-project", line 1, characters 11-17:
  1 | (lang dune 3.18.2)
                 ^^^^^^
  Error: Expected VERSION.MINOR format, got: "3.18.2".
  [123]

  $ cat > dune-project <<EOF
  > (lang invalid 3.18)
  > EOF

  $ dunolint lint
  File "dune-project", line 1, characters 0-19:
  1 | (lang invalid 3.18)
      ^^^^^^^^^^^^^^^^^^^
  Error: Expected (lang dune VERSION) format.
  [123]
