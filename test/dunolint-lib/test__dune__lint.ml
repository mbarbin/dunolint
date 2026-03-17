(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Lint.Predicate.equal in
  let pps_a = `pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_equal"))) in
  let pps_b = `pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_sexp_conv"))) in
  (* Physical equality. *)
  require (equal pps_a pps_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_equal"))))
       (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_equal")))));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal pps_a pps_b));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Lint.Predicate) p in
  test (pps (pp (Dune.Pp.Name.v "ppx_equal")));
  [%expect {| (pps (pp ppx_equal)) |}];
  ()
;;
