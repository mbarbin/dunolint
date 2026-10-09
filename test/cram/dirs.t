Canonical ordering of the dirs stanza.

  $ touch dune-workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

The directories are sorted, keeping the comments and sections of the stanza. The
result shown is the one of linting the file, which includes its auto-formatting.

  $ cat > dune <<EOF
  > (dirs
  >  ; Code.
  >  src
  >  lib ; Shared code.
  > 
  >  ; Tests.
  >  test
  >  bench)
  > EOF

  $ dunolint tools lint-file dune
  (dirs
   ; Code.
   lib ; Shared code.
   src
   ; Tests.
   bench
   test)

Set operations are supported: the directories are sorted on each side of the
difference, but not across it.

  $ cat > dune <<EOF
  > (dirs foo :standard \ test* bench)
  > EOF

  $ dunolint tools lint-file dune
  (dirs :standard foo \ bench test*)
