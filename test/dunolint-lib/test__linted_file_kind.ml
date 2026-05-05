(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "all" =
  List.iter Dunolint.Linted_file_kind.all ~f:(fun linted_file_kind ->
    let sexp = [%sexp (linted_file_kind : Dunolint.Linted_file_kind.t)] in
    let t = Dunolint.Linted_file_kind.t_of_sexp sexp in
    require_equal (module Dunolint.Linted_file_kind) linted_file_kind t;
    print_s sexp);
  [%expect
    {|
    dune
    dune_project
    dune_workspace
    dunolint
    |}];
  ()
;;

let%expect_test "to_string" =
  List.iter Dunolint.Linted_file_kind.all ~f:(fun linted_file_kind ->
    let str = Dunolint.Linted_file_kind.to_string linted_file_kind in
    print_endline str);
  [%expect
    {|
    dune
    dune-project
    dune-workspace
    dunolint
    |}];
  ()
;;

let%expect_test "sort" =
  let sort ts = List.sort ts ~compare:Dunolint.Linted_file_kind.compare in
  let test ts = print_s [%sexp (sort ts : Dunolint.Linted_file_kind.t list)] in
  test [ `dune_project; `dune ];
  [%expect {| (dune dune_project) |}];
  require
    (List.equal
       Dunolint.Linted_file_kind.equal
       Dunolint.Linted_file_kind.all
       (sort Dunolint.Linted_file_kind.all));
  [%expect {||}];
  ()
;;
