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

let%expect_test "all" =
  List.iter Dunolint.Linted_file_kind.all ~f:(fun linted_file_kind ->
    let sexp = [%sexp (linted_file_kind : Dunolint.Linted_file_kind.t)] in
    let t = Dunolint.Linted_file_kind.t_of_sexp sexp in
    require_equal [%here] (module Dunolint.Linted_file_kind) linted_file_kind t;
    print_s sexp);
  [%expect
    {|
    dune
    dune_project
    |}];
  ()
;;

let%expect_test "to_string/of_string" =
  List.iter Dunolint.Linted_file_kind.all ~f:(fun linted_file_kind ->
    let str = Dunolint.Linted_file_kind.to_string linted_file_kind in
    let t =
      match Dunolint.Linted_file_kind.of_string str with
      | Ok v -> v
      | Error (`Msg _) -> assert false
    in
    require_equal [%here] (module Dunolint.Linted_file_kind) linted_file_kind t;
    print_endline str);
  [%expect
    {|
    dune
    dune-project
    |}];
  let () =
    match Dunolint.Linted_file_kind.of_string "invalid" with
    | Ok _ -> assert false
    | Error (`Msg _) as error ->
      print_s [%sexp (error : (_, [ `Msg of string ]) Result.t)]
  in
  [%expect {| (Error (Msg "Invalid linted file kind: \"invalid\"")) |}];
  ()
;;
