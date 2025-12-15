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

module Path_in_workspace = Dunolint_engine.Private.Path_in_workspace

let%expect_test "check_escape_path_exn - valid paths" =
  (* Non-escaping paths should pass validation *)
  let test_ok path_str =
    let path = Relative_path.v path_str in
    print_endline (Relative_path.to_string path)
  in
  test_ok "a/b";
  [%expect {| a/b |}];
  test_ok "a/../b";
  [%expect {| b |}];
  test_ok "./";
  [%expect {| ./ |}];
  test_ok "foo/bar/baz.ml";
  [%expect {| foo/bar/baz.ml |}]
;;

let%expect_test "check_escape_path_exn - escaping paths raise" =
  (* Escaping paths should raise [Invalid_argument]. *)
  let test_raise path_str =
    require_does_raise [%here] (fun () : Relative_path.t -> Relative_path.v path_str)
  in
  test_raise "..";
  [%expect
    {| (Invalid_argument "Relative_path.v: path \"..\" escapes above starting point") |}];
  test_raise "../config";
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"../config\" escapes above starting point")
    |}];
  test_raise "a/../..";
  (* Normalizes to ".." *)
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"a/../..\" escapes above starting point")
    |}]
;;

let%expect_test "parent - basic cases" =
  let test path_str =
    let path = Relative_path.v path_str in
    print_s [%sexp (Relative_path.parent path : Relative_path.t option)]
  in
  (* Empty path has no parent *)
  test "./";
  [%expect {| () |}];
  (* Single file has empty parent *)
  test "file.ml";
  [%expect {| (./) |}];
  (* Directory path *)
  test "a/b/c.ml";
  [%expect {| (a/b/) |}];
  test "a/b/";
  [%expect {| (a/) |}];
  test "a/";
  [%expect {| (./) |}]
;;

let%expect_test "parent - escaping paths raise" =
  let test path_str =
    require_does_raise [%here] (fun () : Relative_path.t option ->
      (Relative_path.parent (Relative_path.v path_str) [@coverage off]))
  in
  test "..";
  [%expect
    {| (Invalid_argument "Relative_path.v: path \"..\" escapes above starting point") |}];
  test "../..";
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"../..\" escapes above starting point")
    |}];
  test "../../config";
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"../../config\" escapes above starting point")
    |}]
;;

let%expect_test "parent - does not create escaping paths" =
  (* This test verifies that parent returns None for empty rather than creating "../" *)
  let rec walk_up path depth =
    if depth > 5
    then
      (* Walked up more than 5 times without reaching None! *)
      assert false [@coverage off]
    else (
      match Relative_path.parent path with
      | None ->
        print_endline
          (Printf.sprintf
             "Reached None at depth %d, path: %s"
             depth
             (Relative_path.to_string path))
      | Some parent ->
        print_endline
          (Printf.sprintf "Depth %d: %s" depth (Relative_path.to_string parent));
        walk_up parent (depth + 1))
  in
  walk_up (Relative_path.v "a/b/c.ml") 0;
  [%expect
    {|
    Depth 0: a/b/
    Depth 1: a/
    Depth 2: ./
    Reached None at depth 3, path: ./ |}]
;;

let%expect_test "chop_prefix - basic cases" =
  let test ~path ~prefix =
    let path = Relative_path.v path in
    let prefix = Relative_path.v prefix in
    print_s [%sexp (Relative_path.chop_prefix path ~prefix : Relative_path.t option)]
  in
  (* Exact prefix *)
  test ~path:"a/b/c.ml" ~prefix:"a/";
  [%expect {| (b/c.ml) |}];
  test ~path:"a/b/c.ml" ~prefix:"a/b/";
  [%expect {| (c.ml) |}];
  (* Empty prefix returns original *)
  test ~path:"a/b/c.ml" ~prefix:"./";
  [%expect {| (a/b/c.ml) |}];
  (* Not a prefix *)
  test ~path:"a/b/c.ml" ~prefix:"x/";
  [%expect {| () |}];
  (* Partial segment is not a prefix *)
  test ~path:"foo/bar-baz" ~prefix:"foo/bar";
  [%expect {| () |}]
;;

let%expect_test "ancestors_autoloading_dirs - from documentation" =
  let test path_str =
    let path = Relative_path.v path_str in
    let ancestors = Path_in_workspace.ancestors_autoloading_dirs ~path in
    List.iter ancestors ~f:(fun p -> print_endline (Relative_path.to_string p))
  in
  (* Example from documentation *)
  test "a/b/c.ml";
  [%expect
    {|
    ./
    a/
    a/b/
    |}];
  test "file.ml";
  [%expect {| ./ |}];
  test "./";
  [%expect {| |}]
;;

let%expect_test "ancestors_autoloading_dirs - ordering" =
  let test path_str =
    let path = Relative_path.v path_str in
    let ancestors = Path_in_workspace.ancestors_autoloading_dirs ~path in
    print_s [%sexp { path : Relative_path.t; ancestors : Relative_path.t list }]
  in
  (* Verify ordering from root to deepest *)
  test "a/b/c/d/e.ml";
  [%expect {| ((path a/b/c/d/e.ml) (ancestors (./ a/ a/b/ a/b/c/ a/b/c/d/))) |}];
  test "x/y.ml";
  [%expect {| ((path x/y.ml) (ancestors (./ x/))) |}]
;;

let%expect_test "ancestors_autoloading_dirs - escaping paths raise" =
  let test path_str =
    require_does_raise [%here] (fun () : Relative_path.t list ->
      (Path_in_workspace.ancestors_autoloading_dirs
         ~path:(Relative_path.v path_str) [@coverage off]))
  in
  test "..";
  [%expect
    {| (Invalid_argument "Relative_path.v: path \"..\" escapes above starting point") |}];
  test "../config/file.ml";
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"../config/file.ml\" escapes above starting point")
    |}]
;;

let%expect_test "ancestors_autoloading_dirs - complex paths" =
  let test path_str =
    let path = Relative_path.v path_str in
    let ancestors = Path_in_workspace.ancestors_autoloading_dirs ~path in
    print_endline (Printf.sprintf "%s: %d ancestors" path_str (List.length ancestors));
    List.iter ancestors ~f:(fun p ->
      print_endline (Printf.sprintf "  - %s" (Relative_path.to_string p)))
  in
  (* Deep nesting *)
  test "lib/dunolint_engine/src/dunolint_engine.ml";
  [%expect
    {|
    lib/dunolint_engine/src/dunolint_engine.ml: 4 ancestors
      - ./
      - lib/
      - lib/dunolint_engine/
      - lib/dunolint_engine/src/
    |}];
  (* Directory path (with trailing slash) *)
  test "a/b/c/";
  [%expect
    {|
    a/b/c/: 3 ancestors
      - ./
      - a/
      - a/b/
    |}]
;;

let%expect_test "paths_to_check_for_skip_predicates - matches CLI behavior" =
  let test path =
    print_s
      [%sexp
        (Path_in_workspace.paths_to_check_for_skip_predicates ~path:(Relative_path.v path)
         : Relative_path.t list)]
  in
  (* Root and normalization *)
  test "./";
  [%expect {| () |}];
  test ".";
  [%expect {| () |}];
  (* File in root - includes the file itself *)
  test "foo";
  [%expect {| (foo) |}];
  (* Directory in root - includes itself *)
  test "foo/";
  [%expect {| (foo/) |}];
  (* File - returns parents and the file itself *)
  test "foo/bar";
  [%expect {| (foo/ foo/bar) |}];
  test "foo/bar/bin";
  [%expect {| (foo/ foo/bar/ foo/bar/bin) |}];
  (* Directory - includes itself AND parents *)
  test "foo/bar/bin/";
  [%expect {| (foo/ foo/bar/ foo/bar/bin/) |}];
  (* More files *)
  test "foo/bar/bin/baz";
  [%expect {| (foo/ foo/bar/ foo/bar/bin/ foo/bar/bin/baz) |}];
  test "foo/bar/.";
  [%expect {| (foo/ foo/bar/) |}];
  ()
;;

let%expect_test "paths_to_check_for_skip_predicates - escaping paths raise" =
  let test path_str =
    require_does_raise [%here] (fun () : Relative_path.t list ->
      (Path_in_workspace.paths_to_check_for_skip_predicates
         ~path:(Relative_path.v path_str) [@coverage off]))
  in
  test "..";
  [%expect
    {| (Invalid_argument "Relative_path.v: path \"..\" escapes above starting point") |}];
  test "../config";
  [%expect
    {|
    (Invalid_argument
     "Relative_path.v: path \"../config\" escapes above starting point")
    |}]
;;
