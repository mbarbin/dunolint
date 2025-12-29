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
  test "pkg_UPPERCASE";
  [%expect {| (Ok pkg_UPPERCASE) |}];
  test "pkg.dot";
  [%expect {| (Error (Msg "\"pkg.dot\": invalid Package_name")) |}];
  test "pkg#sharp";
  [%expect {| (Error (Msg "\"pkg#sharp\": invalid Package_name")) |}];
  test "pkg@at";
  [%expect {| (Error (Msg "\"pkg@at\": invalid Package_name")) |}];
  ()
;;
