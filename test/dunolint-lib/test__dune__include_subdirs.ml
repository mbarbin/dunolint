(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Mode.equal" =
  let equal = Dune.Include_subdirs.Mode.equal in
  (* Structural equality - same variant. *)
  require (equal `no `no);
  [%expect {||}];
  require (equal `unqualified `unqualified);
  [%expect {||}];
  require (equal `qualified `qualified);
  [%expect {||}];
  (* Different variants. *)
  require (not (equal `no `unqualified));
  [%expect {||}];
  ()
;;

let%expect_test "Predicate.equal" =
  let equal = Dune.Include_subdirs.Predicate.equal in
  let equals_no = `equals `no in
  let equals_unqualified = `equals `unqualified in
  (* Physical equality. *)
  require (equal equals_no equals_no);
  [%expect {||}];
  (* Structural equality - same variant, same inner value. *)
  require (equal (`equals `no) (`equals `no));
  [%expect {||}];
  require (equal (`equals `unqualified) (`equals `unqualified));
  [%expect {||}];
  require (equal (`equals `qualified) (`equals `qualified));
  [%expect {||}];
  (* Same variant, different inner value. *)
  require (not (equal equals_no equals_unqualified));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Include_subdirs.Predicate) p in
  test (equals `no);
  [%expect {| (equals no) |}];
  test (equals `unqualified);
  [%expect {| (equals unqualified) |}];
  test (equals `qualified);
  [%expect {| (equals qualified) |}];
  ()
;;
