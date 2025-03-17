This is a regression test to monitor the behavior of the [lint] command on an
example found in the merlin code base, where a flag would be unexpectedly
rewritten.

The original code can be found here:

repo: [https://github.com/ocaml/merlin]
path: [src/ocaml/merlin_specific/dune]
rev : [f358bb6201d57b1b5feb501054144ac683710554]

  $ cat > dune <<EOF
  > (library
  >  (name merlin_specific)
  >  (public_name merlin-lib.ocaml_merlin_specific)
  >  (flags
  >   :standard
  >   -open Ocaml_utils
  >   -open Ocaml_parsing
  >   -open Ocaml_preprocess
  >   -open Ocaml_typing
  >   -open Ocaml_preprocess
  >   -open Merlin_utils)
  >  (libraries merlin_utils ocaml_parsing ocaml_preprocess ocaml_typing ocaml_utils))
  > EOF

The issue is visible in this test. See below how the argument to the [-open]
flag [Ocaml_preprocess] is unexpectedly replaced by [Merlin_utils], doing so
duplicating the value already present at the next line.

  $ dunolint lint
  Editing file "dune":
  -4,9 +4,9
     (flags
      :standard
      -open Ocaml_utils
      -open Ocaml_parsing
      -open Ocaml_preprocess
      -open Ocaml_typing
  -|  -open Ocaml_preprocess
  +|  -open Merlin_utils
      -open Merlin_utils)
     (libraries merlin_utils ocaml_parsing ocaml_preprocess ocaml_typing ocaml_utils))

We are keeping this test as regression test.

See also: [https://github.com/mbarbin/dunolint/issues/18].
