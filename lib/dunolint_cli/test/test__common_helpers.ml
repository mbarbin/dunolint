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

module Common_helpers = Dunolint_cli.Private.Common_helpers

let%expect_test "clean_up_error_message" =
  let test input =
    let output = Common_helpers.clean_up_error_message input in
    print_endline (output : string)
  in
  (* Test the main transformation pattern. *)
  test "lib/dunolint/src/config_v0.ml.T.t_of_sexp: record conversion: only pairs expected";
  [%expect {| config_v0.T: record conversion: only pairs expected |}];
  (* Test with different paths. *)
  test "src/config.ml.Config.t_of_sexp: invalid format";
  [%expect {| config.Config: invalid format |}];
  (* Test with complex nested paths. *)
  test "lib/foo/bar/baz/module_name.ml.Module.function_name: some error";
  [%expect {| module_name.Module: some error |}];
  (* Test strings that don't match the pattern - should remain unchanged. *)
  test "simple error message";
  [%expect {| simple error message |}];
  test "Error: something went wrong";
  [%expect {| Error: something went wrong |}];
  (* Test edge cases. *)
  test "";
  [%expect {||}];
  test "file.ml.Module.func: error";
  [%expect {| file.Module: error |}];
  (* Test with various extensions - only .ml should match. *)
  test "lib/test.mli.Module.func: error";
  [%expect {| lib/test.mli.Module.func: error |}];
  ()
;;
