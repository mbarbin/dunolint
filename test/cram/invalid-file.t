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

  $ dunolint lint --check
  File "dune", line 3, characters 0-0:
  Error: unclosed parentheses at end of input
  
  File "dune-project", line 1, characters 11-11:
  1 | (name main))
                 
  Error: unexpected character: ')'
  [123]
