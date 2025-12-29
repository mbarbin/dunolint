(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

let%expect_test "to_string" =
  let version = Dune_project.Dune_lang_version.create (3, 20) in
  print_endline (Dune_project.Dune_lang_version.to_string version);
  [%expect {| 3.20 |}]
;;

let%expect_test "to_string various versions" =
  let versions = [ 1, 0; 2, 15; 3, 20; 4, 5 ] in
  List.iter versions ~f:(fun v ->
    let version = Dune_project.Dune_lang_version.create v in
    print_endline (Dune_project.Dune_lang_version.to_string version));
  [%expect
    {|
    1.0
    2.15
    3.20
    4.5 |}]
;;

let%expect_test "of_sexp" =
  let test sexp =
    match Dune_project.Dune_lang_version.t_of_sexp sexp with
    | t -> print_endline (Dune_project.Dune_lang_version.to_string t)
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  test (Atom "3.19");
  [%expect {| 3.19 |}];
  test (List []);
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp ())) |}];
  test (List [ Atom "3"; Atom "19" ]);
  [%expect
    {|
    (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]."
     (invalid_sexp (3 19)))
    |}];
  test (Atom "");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp "")) |}];
  test (Atom ".");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp .)) |}];
  test (Atom "3.");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp 3.)) |}];
  test (Atom "3");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp 3)) |}];
  test (Atom "3.19.1");
  [%expect
    {|
    (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]."
     (invalid_sexp 3.19.1))
    |}];
  ()
;;

let%expect_test "predicate equals" =
  let version = Dune_project.Dune_lang_version.create (3, 20) in
  let predicate = `equals version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (equals 3.20) |}]
;;

let%expect_test "predicate greater_than_or_equal_to" =
  let version = Dune_project.Dune_lang_version.create (3, 18) in
  let predicate = `greater_than_or_equal_to version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (greater_than_or_equal_to 3.18) |}]
;;

let%expect_test "predicate less_than_or_equal_to" =
  let version = Dune_project.Dune_lang_version.create (4, 0) in
  let predicate = `less_than_or_equal_to version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (less_than_or_equal_to 4.0) |}]
;;

let%expect_test "predicate eq" =
  let version = Dune_project.Dune_lang_version.create (3, 20) in
  let predicate = `eq version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (= 3.20) |}]
;;

let%expect_test "predicate neq" =
  let version = Dune_project.Dune_lang_version.create (3, 20) in
  let predicate = `neq version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (!= 3.20) |}]
;;

let%expect_test "predicate gte" =
  let version = Dune_project.Dune_lang_version.create (3, 18) in
  let predicate = `gte version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (>= 3.18) |}]
;;

let%expect_test "predicate gt" =
  let version = Dune_project.Dune_lang_version.create (3, 18) in
  let predicate = `gt version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (> 3.18) |}]
;;

let%expect_test "predicate lte" =
  let version = Dune_project.Dune_lang_version.create (4, 0) in
  let predicate = `lte version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (<= 4.0) |}]
;;

let%expect_test "predicate lt" =
  let version = Dune_project.Dune_lang_version.create (4, 0) in
  let predicate = `lt version in
  print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)];
  [%expect {| (< 4.0) |}]
;;

let%expect_test "Predicate.t_of_sexp" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match Dune_project.Dune_lang_version.Predicate.t_of_sexp sexp with
    | predicate ->
      print_s [%sexp (predicate : Dune_project.Dune_lang_version.Predicate.t)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  test "(= 3.20)";
  [%expect {| (= 3.20) |}];
  test "(!= 3.20)";
  [%expect {| (!= 3.20) |}];
  test "(>= 3.18)";
  [%expect {| (>= 3.18) |}];
  test "(> 3.18)";
  [%expect {| (> 3.18) |}];
  test "(<= 4.0)";
  [%expect {| (<= 4.0) |}];
  test "(< 4.0)";
  [%expect {| (< 4.0) |}];
  (* Deprecated operators - parsed and normalized to new operators. *)
  test "(equals 3.20)";
  [%expect {| (= 3.20) |}];
  test "(greater_than_or_equal_to 3.18)";
  [%expect {| (>= 3.18) |}];
  test "(less_than_or_equal_to 4.0)";
  [%expect {| (<= 4.0) |}];
  (* Error cases. *)
  test "=";
  [%expect
    {|
    (Of_sexp_error
     "dune_lang_version.t_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp =))
    |}];
  test "(= 3.20 extra)";
  [%expect
    {|
    (Of_sexp_error
     "dune_lang_version.t_of_sexp: polymorphic variant tag \"=\" has incorrect number of arguments"
     (invalid_sexp (= 3.20 extra)))
    |}];
  test "(unknown 3.20)";
  [%expect
    {|
    (Of_sexp_error "dune_lang_version.t_of_sexp: no matching variant found"
     (invalid_sexp (unknown 3.20)))
    |}];
  ()
;;

let%expect_test "equal comparison" =
  let v1 = Dune_project.Dune_lang_version.create (3, 20) in
  let v2 = Dune_project.Dune_lang_version.create (3, 20) in
  let v3 = Dune_project.Dune_lang_version.create (3, 15) in
  print_s [%sexp (Dune_project.Dune_lang_version.equal v1 v2 : bool)];
  print_s [%sexp (Dune_project.Dune_lang_version.equal v1 v3 : bool)];
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "compare versions" =
  (* Keep specific compare tests for regression testing of individual values. *)
  let v1 = Dune_project.Dune_lang_version.create (3, 15) in
  let v2 = Dune_project.Dune_lang_version.create (3, 20) in
  let v3 = Dune_project.Dune_lang_version.create (4, 0) in
  print_s [%sexp (Dune_project.Dune_lang_version.compare v1 v2 : int)];
  print_s [%sexp (Dune_project.Dune_lang_version.compare v2 v3 : int)];
  print_s [%sexp (Dune_project.Dune_lang_version.compare v2 v2 : int)];
  [%expect
    {|
    -1
    -1
    0 |}]
;;

let%expect_test "sort versions" =
  (* Test compare function by sorting an initially unordered list. *)
  let unsorted_versions =
    [ 4, 1; 3, 15; 4, 0; 2, 8; 3, 20; 1, 12 ]
    |> List.map ~f:Dune_project.Dune_lang_version.create
  in
  let sorted_versions =
    List.sort unsorted_versions ~compare:Dune_project.Dune_lang_version.compare
  in
  print_s [%sexp (sorted_versions : Dune_project.Dune_lang_version.t list)];
  [%expect {| (1.12 2.8 3.15 3.20 4.0 4.1) |}]
;;
