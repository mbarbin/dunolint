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
        (Dune.Instrumentation.Backend.Name.of_string str
         : (Dune.Instrumentation.Backend.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Dunolint.Instrumentation.Backend.Name")) |}];
  test "bisect_ppx";
  [%expect {| (Ok bisect_ppx) |}];
  test "backend-dash";
  [%expect
    {| (Error (Msg "\"backend-dash\": invalid Dunolint.Instrumentation.Backend.Name")) |}];
  test "backend_underscore";
  [%expect {| (Ok backend_underscore) |}];
  test "backend.dot";
  [%expect {| (Ok backend.dot) |}];
  test "backend#sharp";
  [%expect
    {|
    (Error (
      Msg "\"backend#sharp\": invalid Dunolint.Instrumentation.Backend.Name"))
    |}];
  test "backend@at";
  [%expect
    {| (Error (Msg "\"backend@at\": invalid Dunolint.Instrumentation.Backend.Name")) |}];
  ()
;;

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Instrumentation.Predicate) p in
  test (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"));
  [%expect {| (backend bisect_ppx) |}];
  ()
;;
