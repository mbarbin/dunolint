(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Library.Package.Predicate.equal in
  let equals_a = `equals (Dune.Package.Name.v "pkg_a") in
  let equals_b = `equals (Dune.Package.Name.v "pkg_b") in
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
       (`equals (Dune.Package.Name.v "pkg_a"))
       (`equals (Dune.Package.Name.v "pkg_a")));
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
        (Dune.Package.Name.of_string str
         : (Dune.Package.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Package_name")) |}];
  test "pkg";
  [%expect {| (Ok pkg) |}];
  test "pkg-dash";
  [%expect {| (Ok pkg-dash) |}];
  test "pkg_underscore";
  [%expect {| (Ok pkg_underscore) |}];
  test "pkg.dot";
  [%expect {| (Error (Msg "\"pkg.dot\": invalid Package_name")) |}];
  test "pkg#sharp";
  [%expect {| (Error (Msg "\"pkg#sharp\": invalid Package_name")) |}];
  test "pkg@at";
  [%expect {| (Error (Msg "\"pkg@at\": invalid Package_name")) |}];
  ()
;;

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Library.Package.Predicate) p in
  test (equals (Dune.Package.Name.v "pkg"));
  [%expect {| (equals pkg) |}];
  test (is_prefix "prefix");
  [%expect {| (is_prefix prefix) |}];
  test (is_suffix "suffix");
  [%expect {| (is_suffix suffix) |}];
  ()
;;
