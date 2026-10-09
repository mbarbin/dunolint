(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Dunolint.Config.Std

let parse contents =
  Test_helpers.parse (module Dune_linter.Data_only_dirs) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Data_only_dirs.write t);
      print_s (Dune_linter.Data_only_dirs.sexp_of_t t))
  in
  test {| (data_only_dirs foo bar) |};
  [%expect
    {|
    (data_only_dirs foo bar)
    ((dirs foo bar))
    |}];
  (* The directories are atoms. *)
  test {| (data_only_dirs foo (bar)) |};
  [%expect
    {|
    File "dune", line 1, characters 21-26:
    Error: Atom or quoted string expected.
    [123]
    |}];
  ()
;;

let rewrite str =
  let (sexps_rewriter, field), t = parse str in
  Dune_linter.Data_only_dirs.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  (* The directories are sorted alphabetically, globs and quoted names included. *)
  rewrite {| (data_only_dirs test* "bin" foo) |};
  [%expect {| (data_only_dirs "bin" foo test*) |}];
  rewrite {| (data_only_dirs bin foo) |};
  [%expect {| (data_only_dirs bin foo) |}];
  (* Comments are kept, as for the [dirs] stanza. *)
  rewrite
    {|(data_only_dirs
 ; Fixtures.
 test_data
 examples ; Not built.

 ; Docs.
 doc)|};
  [%expect
    {|
    (data_only_dirs
     ; Fixtures.
     examples ; Not built.
     test_data

     ; Docs.
     doc)
    |}];
  ()
;;

let%expect_test "eval and enforce" =
  (* There are no predicates for this stanza. *)
  let _, t = parse {| (data_only_dirs foo) |} in
  Dune_linter.Data_only_dirs.enforce t ~condition:true_;
  [%expect {||}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (data_only_dirs foo) |} in
  Test_helpers.is_false
    (Dune_linter.Data_only_dirs.Linter.eval t ~predicate:(`stanza (Blang.base `library)));
  Test_helpers.is_undefined
    (Dune_linter.Data_only_dirs.Linter.eval t ~predicate:(`library true_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let _, t = parse {| (data_only_dirs foo) |} in
  let apply condition =
    Dunolinter.Handler.raise ~f:(fun () ->
      Dune_linter.Data_only_dirs.Linter.enforce t ~condition)
  in
  apply (not_ (stanza (Blang.base `library)));
  apply (library true_);
  [%expect {||}];
  require_does_raise (fun () -> apply (stanza (Blang.base `library)));
  [%expect {| (Dunolinter.Handler.Enforce_failure (condition (stanza library))) |}];
  ()
;;
