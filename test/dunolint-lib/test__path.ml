(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "Predicate.equal" =
  let equal = Dunolint.Path.Predicate.equal in
  let glob_a = `glob (Dunolint.Glob.v ".git/*") in
  let glob_b = `glob (Dunolint.Glob.v "src/*") in
  (* Physical equality. *)
  require (equal glob_a glob_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`glob (Dunolint.Glob.v ".git/*")) (`glob (Dunolint.Glob.v ".git/*")));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal glob_a glob_b));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dunolint.Path.Predicate) p in
  test (glob ".git/*");
  [%expect {| (glob .git/*) |}];
  ()
;;
