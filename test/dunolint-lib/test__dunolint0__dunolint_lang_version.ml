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
  let version = Dunolint0.Dunolint_lang_version.create (1, 0) in
  print_endline (Dunolint0.Dunolint_lang_version.to_string version);
  [%expect {| 1.0 |}]
;;

let%expect_test "to_string various versions" =
  let versions = [ 1, 0; 1, 5; 2, 0; 2, 10 ] in
  List.iter versions ~f:(fun v ->
    let version = Dunolint0.Dunolint_lang_version.create v in
    print_endline (Dunolint0.Dunolint_lang_version.to_string version));
  [%expect
    {|
    1.0
    1.5
    2.0
    2.10 |}]
;;

let%expect_test "of_sexp" =
  let test sexp =
    match Dunolint0.Dunolint_lang_version.t_of_sexp sexp with
    | t -> print_endline (Dunolint0.Dunolint_lang_version.to_string t)
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  test (Atom "1.0");
  [%expect {| 1.0 |}];
  test (List []);
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp ())) |}];
  test (List [ Atom "1"; Atom "0" ]);
  [%expect
    {|
    (Of_sexp_error
     "Invalid version - expected [MAJOR.MINOR]."
     (invalid_sexp (1 0)))
    |}];
  test (Atom "");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp "")) |}];
  test (Atom ".");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp .)) |}];
  test (Atom "1.");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp 1.)) |}];
  test (Atom "1");
  [%expect
    {| (Of_sexp_error "Invalid version - expected [MAJOR.MINOR]." (invalid_sexp 1)) |}];
  test (Atom "1.0.1");
  [%expect
    {|
    (Of_sexp_error
     "Invalid version - expected [MAJOR.MINOR]."
     (invalid_sexp 1.0.1))
    |}];
  ()
;;

let%expect_test "predicate eq" =
  let version = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let predicate = `eq version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (= 1.0) |}]
;;

let%expect_test "predicate neq" =
  let version = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let predicate = `neq version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (!= 1.0) |}]
;;

let%expect_test "predicate gte" =
  let version = Dunolint0.Dunolint_lang_version.create (1, 5) in
  let predicate = `gte version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (>= 1.5) |}]
;;

let%expect_test "predicate gt" =
  let version = Dunolint0.Dunolint_lang_version.create (1, 5) in
  let predicate = `gt version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (> 1.5) |}]
;;

let%expect_test "predicate lte" =
  let version = Dunolint0.Dunolint_lang_version.create (2, 0) in
  let predicate = `lte version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (<= 2.0) |}]
;;

let%expect_test "predicate lt" =
  let version = Dunolint0.Dunolint_lang_version.create (2, 0) in
  let predicate = `lt version in
  print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)];
  [%expect {| (< 2.0) |}]
;;

let%expect_test "Predicate.t_of_sexp" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match Dunolint0.Dunolint_lang_version.Predicate.t_of_sexp sexp with
    | predicate ->
      print_s [%sexp (predicate : Dunolint0.Dunolint_lang_version.Predicate.t)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  test "(= 1.0)";
  [%expect {| (= 1.0) |}];
  test "(!= 1.0)";
  [%expect {| (!= 1.0) |}];
  test "(>= 1.5)";
  [%expect {| (>= 1.5) |}];
  test "(> 1.5)";
  [%expect {| (> 1.5) |}];
  test "(<= 2.0)";
  [%expect {| (<= 2.0) |}];
  test "(< 2.0)";
  [%expect {| (< 2.0) |}];
  (* Error cases. *)
  test "=";
  [%expect
    {|
    (Of_sexp_error
     "dunolint_lang_version.t_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp =))
    |}];
  test "(= 1.0 extra)";
  [%expect
    {|
    (Of_sexp_error
     "dunolint_lang_version.t_of_sexp: polymorphic variant tag \"=\" has incorrect number of arguments"
     (invalid_sexp (= 1.0 extra)))
    |}];
  test "(unknown 1.0)";
  [%expect
    {|
    (Of_sexp_error
     "dunolint_lang_version.t_of_sexp: no matching variant found"
     (invalid_sexp (unknown 1.0)))
    |}];
  ()
;;

let%expect_test "equal comparison" =
  let v1 = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let v2 = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let v3 = Dunolint0.Dunolint_lang_version.create (1, 5) in
  print_s [%sexp (Dunolint0.Dunolint_lang_version.equal v1 v2 : bool)];
  print_s [%sexp (Dunolint0.Dunolint_lang_version.equal v1 v3 : bool)];
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "compare versions" =
  (* Keep specific compare tests for regression testing of individual values. *)
  let v1 = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let v2 = Dunolint0.Dunolint_lang_version.create (1, 5) in
  let v3 = Dunolint0.Dunolint_lang_version.create (2, 0) in
  print_s [%sexp (Dunolint0.Dunolint_lang_version.compare v1 v2 : int)];
  print_s [%sexp (Dunolint0.Dunolint_lang_version.compare v2 v3 : int)];
  print_s [%sexp (Dunolint0.Dunolint_lang_version.compare v2 v2 : int)];
  [%expect
    {|
    -1
    -1
    0 |}]
;;

let%expect_test "sort versions" =
  let unsorted_versions =
    [ 2, 1; 1, 5; 2, 0; 1, 0; 1, 10; 1, 2 ]
    |> List.map ~f:Dunolint0.Dunolint_lang_version.create
  in
  let sorted_versions =
    List.sort unsorted_versions ~compare:Dunolint0.Dunolint_lang_version.compare
  in
  print_s [%sexp (sorted_versions : Dunolint0.Dunolint_lang_version.t list)];
  [%expect {| (1.0 1.2 1.5 1.10 2.0 2.1) |}]
;;
