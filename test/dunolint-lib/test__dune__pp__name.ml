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

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp (Dune.Pp.Name.of_string str : (Dune.Pp.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Pp_name")) |}];
  test "pp";
  [%expect {| (Ok pp) |}];
  test "pp-dash";
  [%expect {| (Ok pp-dash) |}];
  test "pp_underscore";
  [%expect {| (Ok pp_underscore) |}];
  test "pp.dot";
  [%expect {| (Ok pp.dot) |}];
  test "pp#sharp";
  [%expect {| (Error (Msg "\"pp#sharp\": invalid Pp_name")) |}];
  test "pp@at";
  [%expect {| (Error (Msg "\"pp@at\": invalid Pp_name")) |}];
  ()
;;

let%expect_test "compare" =
  let test vs =
    let vs = List.map vs ~f:Dune.Pp.Name.v in
    print_s [%sexp (List.sort vs ~compare:Dune.Pp.Name.compare : Dune.Pp.Name.t list)]
  in
  test [ "a"; "b" ];
  [%expect {| (a b) |}];
  test [ "ppx_this"; "that"; "ppx_that"; "lib.ppx_bcd"; "otherlib.ppx_abc" ];
  [%expect {| (otherlib.ppx_abc lib.ppx_bcd ppx_that ppx_this that) |}];
  ()
;;
