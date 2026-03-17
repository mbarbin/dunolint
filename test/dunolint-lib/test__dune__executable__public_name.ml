(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Executable.Public_name.Predicate.equal in
  let equals_a = `equals (Dune.Executable.Public_name.v "main_a") in
  let equals_b = `equals (Dune.Executable.Public_name.v "main_b") in
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
       (`equals (Dune.Executable.Public_name.v "main_a"))
       (`equals (Dune.Executable.Public_name.v "main_a")));
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
        (Dune.Executable.Public_name.of_string str
         : (Dune.Executable.Public_name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Dunolint.Executable.Public_name")) |}];
  test "main";
  [%expect {| (Ok main) |}];
  test "main-dash";
  [%expect {| (Ok main-dash) |}];
  test "main_underscore";
  [%expect {| (Ok main_underscore) |}];
  test "main.dot";
  [%expect {| (Ok main.dot) |}];
  test "main#sharp";
  [%expect {| (Error (Msg "\"main#sharp\": invalid Dunolint.Executable.Public_name")) |}];
  test "main@at";
  [%expect {| (Error (Msg "\"main@at\": invalid Dunolint.Executable.Public_name")) |}];
  ()
;;

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Executable.Public_name.Predicate) p in
  test (equals (Dune.Executable.Public_name.v "main"));
  [%expect {| (equals main) |}];
  test (is_prefix "prefix");
  [%expect {| (is_prefix prefix) |}];
  test (is_suffix "suffix");
  [%expect {| (is_suffix suffix) |}];
  ()
;;
