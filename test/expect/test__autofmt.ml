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

let%expect_test "maybe_autoformat_file" =
  let test ~previous_contents ~new_contents =
    let fmt =
      Dunolint_cli.Private.Linter.maybe_autoformat_file
        ~dune_version:(Preset (Dunolint.Dune_project.Dune_lang_version.create (3, 17)))
        ~previous_contents
        ~new_contents
    in
    print_endline fmt
  in
  (* When the previous contents is well formatted, the new contents gets
     formatted as well. *)
  test
    ~previous_contents:
      (String.lstrip
         {|
(lang dune 3.17)

(name dunolint)
|})
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect
    {|
    (lang dune 3.17)

    (name dunolint)
    |}];
  (* If the previous contents is not formatted, then we do not autoformat the
     new one. This is a heuristic. *)
  test
    ~previous_contents:"(lang  dune 3.17) (name     dunolint)"
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect {| (lang    dune 3.17) (name dunolint) |}];
  (* This includes cases where the previous contents fails to auto format. *)
  test
    ~previous_contents:"(lang  dune 3.17 invalid-file"
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect {| (lang    dune 3.17) (name dunolint) |}];
  ()
;;
