(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Dunolint.Config.Std

let parse contents =
  Test_helpers.parse (module Dune_linter.Dirs) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Dirs.write t))
  in
  test {| (dirs foo bar) |};
  [%expect {| (dirs foo bar) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [dirs] field.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (dirs :standard \ foo) |} in
  print_s (t |> Dune_linter.Dirs.sexp_of_t);
  [%expect {| ((dirs (diff (or :standard) (or (element foo))))) |}];
  ()
;;

let rewrite str =
  let (sexps_rewriter, field), t = parse str in
  Dune_linter.Dirs.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  (* The directories are sorted alphabetically. *)
  rewrite {| (dirs foo bar baz) |};
  [%expect {| (dirs bar baz foo) |}];
  rewrite {| (dirs bar baz foo) |};
  [%expect {| (dirs bar baz foo) |}];
  (* Globs and quoted names are sorted along with the other names. *)
  rewrite {| (dirs test* "bin" *.d) |};
  [%expect {| (dirs *.d "bin" test*) |}];
  ()
;;

let%expect_test "rewrite - comments" =
  (* Comments placed on their own line and blank lines delimit sections, which are
     sorted independently. A comment placed after a directory on the same line moves
     along with it. *)
  rewrite
    {|(dirs
 ; Libraries.
 src
 lib ; Shared code.

 ; Tests.
 test
 bench)|};
  [%expect
    {|
    (dirs
     ; Libraries.
     lib ; Shared code.
     src

     ; Tests.
     bench
     test)
    |}];
  ()
;;

let%expect_test "rewrite - predicate language" =
  (* The order follows the model of dune's predicate language (see
     [test__predicate_lang.ml] for the details). The arguments of the stanza are the
     operands of a union: an operator name among them is a directory. *)
  rewrite {| (dirs or (or b a) :standard \ test* foo (:include b a)) |};
  [%expect {| (dirs :standard or (or a b) \ foo test* (:include b a)) |}];
  ()
;;

let%expect_test "eval and enforce" =
  (* There are no predicates for this stanza. *)
  let _, t = parse {| (dirs foo) |} in
  Dune_linter.Dirs.enforce t ~condition:true_;
  [%expect {||}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (dirs foo) |} in
  Test_helpers.is_false
    (Dune_linter.Dirs.Linter.eval t ~predicate:(`stanza (Blang.base `library)));
  [%expect {||}];
  Test_helpers.is_undefined (Dune_linter.Dirs.Linter.eval t ~predicate:(`library true_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let _, t = parse {| (dirs foo) |} in
  let apply condition =
    Dunolinter.Handler.raise ~f:(fun () -> Dune_linter.Dirs.Linter.enforce t ~condition)
  in
  apply (not_ (stanza (Blang.base `library)));
  apply (library true_);
  [%expect {||}];
  require_does_raise (fun () -> apply (stanza (Blang.base `library)));
  [%expect {| (Dunolinter.Handler.Enforce_failure (condition (stanza library))) |}];
  ()
;;
