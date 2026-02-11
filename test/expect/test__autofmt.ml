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

let make_context ~dune_project =
  let dune_project_context =
    let original_contents = String.strip dune_project in
    match
      Dunolint_engine.Dune_project_context.create
        ~path:(Relative_path.v "dune-project")
        ~original_contents
    with
    | Ok _ as ok -> ok
    | Error err ->
      Error (Dunolint_engine.Dune_project_context.Invalid_dune_project.acknowledge err)
  in
  Dunolint_engine.Context.add_dune_project_context
    Dunolint_engine.Context.empty
    ~dune_project_context
    ~location:Relative_path.empty
;;

let%expect_test "autoformat_dune_file" =
  let test ~context ~previous_contents ~new_contents =
    Err.For_test.protect (fun () ->
      let fmt =
        Dunolint_cli.Private.Linter.autoformat_dune_file
          ~context
          ~path:(Relative_path.v "dune-project")
          ~previous_contents
          ~new_contents
      in
      print_endline fmt)
  in
  let context =
    make_context
      ~dune_project:
        {|
(lang dune 3.17)
|}
  in
  (* When the previous contents is well formatted, the new contents gets
     formatted as well. *)
  test
    ~context
    ~previous_contents:
      (String.lstrip
         {|
(lang dune 3.17)

(name dunolint)
|})
    ~new_contents:"(lang dune 3.17)\n(name      dunolint)";
  [%expect
    {|
    (lang dune 3.17)

    (name dunolint)
    |}];
  (* If the previous contents is not formatted, then we do not autoformat the
     new one. This is a heuristic. *)
  test
    ~context
    ~previous_contents:"(lang dune 3.17) (name     dunolint)"
    ~new_contents:"(lang dune 3.17)\n(name    dunolint)";
  [%expect
    {|
    (lang dune 3.17)
    (name    dunolint)
    |}];
  (* This includes cases where the previous contents fails to auto format. *)
  test
    ~context
    ~previous_contents:"(lang dune 3.17 invalid-file"
    ~new_contents:"(lang dune 3.17)\n(name    dunolint)";
  [%expect
    {|
    (lang dune 3.17)
    (name    dunolint)
    |}];
  (* When the enclosing dune-project is invalid, we do not apply formatting. *)
  let context = make_context ~dune_project:"(invalid" in
  test
    ~context
    ~previous_contents:
      (String.lstrip
         {|
(lang dune 3.17)

(name dunolint)
|})
    ~new_contents:"(lang dune 3.17)\n(name\n      dunolint)";
  [%expect
    {|
    (lang dune 3.17)
    (name
          dunolint)
    |}];
  (* When the enclosing dune-project can be parsed by dunolint but does not
     contain a dune lang version, we do not apply formatting. *)
  let context = make_context ~dune_project:"" in
  test
    ~context
    ~previous_contents:
      (String.lstrip
         {|
(lang dune 3.17)

(name dunolint)
|})
    ~new_contents:"(lang dune 3.17)\n(name\n      dunolint)";
  [%expect
    {|
    (lang dune 3.17)
    (name
          dunolint)
    |}];
  ()
;;
