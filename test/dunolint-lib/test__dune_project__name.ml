(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune_project.Name.Predicate.equal in
  let equals_a = `equals (Dune_project.Name.v "proj_a") in
  let equals_b = `equals (Dune_project.Name.v "proj_b") in
  let is_prefix_a = `is_prefix "prefix_a" in
  let is_prefix_b = `is_prefix "prefix_b" in
  let is_suffix_a = `is_suffix "suffix_a" in
  let is_suffix_b = `is_suffix "suffix_b" in
  (* Physical equality. *)
  require (equal equals_a equals_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (`equals (Dune_project.Name.v "proj_a"))
       (`equals (Dune_project.Name.v "proj_a")));
  [%expect {||}];
  require (equal (`is_prefix "prefix_a") (`is_prefix "prefix_a"));
  [%expect {||}];
  require (equal (`is_suffix "suffix_a") (`is_suffix "suffix_a"));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal equals_a equals_b));
  [%expect {||}];
  require (not (equal is_prefix_a is_prefix_b));
  [%expect {||}];
  require (not (equal is_suffix_a is_suffix_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal equals_a is_prefix_a));
  [%expect {||}];
  require (not (equal is_prefix_a is_suffix_a));
  [%expect {||}];
  require (not (equal is_suffix_a equals_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp
        (Dune_project.Name.of_string str
         : (Dune_project.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Dunolint.Dune_project.Name")) |}];
  test "name";
  [%expect {| (Ok name) |}];
  test "name-dash";
  [%expect {| (Ok name-dash) |}];
  test "name_underscore";
  [%expect {| (Ok name_underscore) |}];
  test "name.dot";
  [%expect {| (Ok name.dot) |}];
  test "name#sharp";
  [%expect {| (Error (Msg "\"name#sharp\": invalid Dunolint.Dune_project.Name")) |}];
  test "name@at";
  [%expect {| (Error (Msg "\"name@at\": invalid Dunolint.Dune_project.Name")) |}];
  ()
;;

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune_project.Name.Predicate) p in
  test (equals (Dune_project.Name.v "main"));
  [%expect {| (equals main) |}];
  test (is_prefix "prefix");
  [%expect {| (is_prefix prefix) |}];
  test (is_suffix "suffix");
  [%expect {| (is_suffix suffix) |}];
  ()
;;
