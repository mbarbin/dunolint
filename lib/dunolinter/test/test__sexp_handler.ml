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

module Int_list =
  Dunolinter.Sexp_handler.Make_sexpable_list
    (struct
      let field_name = "ints"
    end)
    (Int)

let%expect_test "rewrite" =
  let test original_contents ~f =
    let sexps_rewriter, field =
      Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
    in
    let t = Int_list.read ~sexps_rewriter ~field in
    Int_list.rewrite (f t) ~sexps_rewriter ~field;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  test {|(ints 1 2 3)|} ~f:(fun t -> 0 :: t);
  [%expect {| (ints 0 1 2 3) |}];
  ()
;;

let%expect_test "insert" =
  let insert original_contents ~indicative_field_ordering ~new_fields =
    let sexps_rewriter =
      match Sexps_rewriter.create ~path:(Fpath.v "file") ~original_contents with
      | Ok r -> r
      | Error { loc; message } -> Err.raise ~loc [ Pp.text message ] [@coverage off]
    in
    let fields = Sexps_rewriter.original_sexps sexps_rewriter in
    Dunolinter.Sexp_handler.insert_new_fields
      ~sexps_rewriter
      ~indicative_field_ordering
      ~fields
      ~new_fields;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  insert {| () |} ~indicative_field_ordering:[] ~new_fields:[];
  [%expect {| () |}];
  insert
    {| (a a) (b b) (c c) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ]; Sexp.List [ Atom "e"; Atom "e" ] ];
  [%expect
    {|
    (a a)
    (d d) (b b)
          (e e) (c c)
    |}];
  insert
    {| (a a) ((c) c) (b b) |}
    ~indicative_field_ordering:[ "a"; "d"; "b" ]
    ~new_fields:[ Sexp.List [ Atom "d"; Atom "d" ] ];
  [%expect
    {|
    (a a)
    (d d) ((c) c) (b b)
    |}];
  ()
;;

let%expect_test "clean_up_error_message" =
  let test input =
    let output = Dunolinter.Sexp_handler.clean_up_error_message input in
    print_endline (output : string)
  in
  (* Test the main transformation pattern. *)
  test "lib/dunolint/src/config_v0.ml.T.t_of_sexp: record conversion: only pairs expected";
  [%expect {| config_v0: record conversion: only pairs expected |}];
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
