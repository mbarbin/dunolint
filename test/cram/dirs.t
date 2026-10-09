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

The data_only_dirs stanza is a plain list of directories, sorted the same way.

  $ cat > dune <<EOF
  > (data_only_dirs test_data examples ; Not built.
  >  doc)
  > EOF

  $ dunolint tools lint-file dune
  (data_only_dirs
   doc
   examples ; Not built.
   test_data)

So is the vendored_dirs stanza.

  $ cat > dune <<EOF
  > (vendored_dirs zarith base)
  > EOF

  $ dunolint tools lint-file dune
  (vendored_dirs base zarith)
