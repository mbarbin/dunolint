(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "equal" =
  let equal = Dunolint.Condition.equal in
  let path_a = Blang.base (`path (Blang.base (`glob (Dunolint.Glob.v "src/*")))) in
  let path_b = Blang.base (`path (Blang.base (`glob (Dunolint.Glob.v "test/*")))) in
  (* Physical equality. *)
  require (equal path_a path_a);
  [%expect {||}];
  (* Structural equality - same value. *)
  require
    (equal
       (Blang.base (`path (Blang.base (`glob (Dunolint.Glob.v "src/*")))))
       (Blang.base (`path (Blang.base (`glob (Dunolint.Glob.v "src/*"))))));
  [%expect {||}];
  (* Different values. *)
  require (not (equal path_a path_b));
  [%expect {||}];
  ()
;;

open! Dunolint.Config.Std

let%expect_test "sexp" =
  require_does_raise (fun () ->
    Dunolint.Condition.t_of_sexp
      (Parsexp.Single.parse_string_exn "(path (equals path/to/file))"));
  [%expect
    {|
    (Of_sexp_error "The [path.equals] construct is no longer supported."
     (invalid_sexp (equals path/to/file)))
    |}];
  ()
;;
