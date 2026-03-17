(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune_project.Generate_opam_files.Predicate.equal in
  (* Structural equality - same variant. *)
  require (equal `is_present `is_present);
  [%expect {||}];
  (* Only one variant, so no different variant tests needed. *)
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p =
    Common.test_predicate (module Dune_project.Generate_opam_files.Predicate) p
  in
  test (Blang.base `is_present);
  [%expect {| is_present |}];
  ()
;;
