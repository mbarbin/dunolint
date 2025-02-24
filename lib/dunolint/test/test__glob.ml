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

module Glob = Dunolint.Glob

let%expect_test "v" =
  let test str =
    match Glob.v str with
    | exception Invalid_argument s -> print_s [%sexp Invalid_argument (s : string)]
    | glob -> print_s [%sexp (glob : Glob.t)]
  in
  test "";
  [%expect {| "" |}];
  test "(";
  [%expect {| "(" |}];
  test "[*";
  [%expect {| (Invalid_argument "Re.Glob.Parse_error: [*") |}];
  ()
;;

let%expect_test "equal" =
  let test str1 str2 =
    print_s [%sexp `equals, (Glob.equal (Glob.v str1) (Glob.v str2) : bool)]
  in
  test "" "";
  [%expect {| (equals true) |}];
  test "a" "";
  [%expect {| (equals false) |}];
  test "a" "a";
  [%expect {| (equals true) |}];
  ()
;;

let%expect_test "roundtrip" =
  let test p = Common.test_roundtrip (module Glob) p in
  test (Glob.v "");
  [%expect {| "" |}];
  test (Glob.v "a");
  [%expect {| a |}];
  test (Glob.v "a/**/b");
  [%expect {| a/**/b |}];
  test (Glob.v ".git/*");
  [%expect {| .git/* |}];
  ()
;;

let%expect_test "test" =
  let test g str = print_s [%sexp `is_match, (Glob.test g str : bool)] in
  let g = Glob.v "const" in
  test g "const";
  [%expect {| (is_match true) |}];
  test g "aconst";
  [%expect {| (is_match false) |}];
  test g "aconstant";
  [%expect {| (is_match false) |}];
  ()
;;

let%expect_test "sort" =
  let test vs =
    let vs = List.map vs ~f:Glob.v |> List.sort ~compare:Glob.compare in
    print_s [%sexp (vs : Glob.t list)]
  in
  test [ "c"; "b/*"; "a/**" ];
  [%expect {| (a/** b/* c) |}];
  ()
;;
