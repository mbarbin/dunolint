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

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dunolint.Predicate) p in
  test (dune (executable (name (equals (Dune.Executable.Name.v "main")))));
  [%expect {| (dune (executable (name (equals main)))) |}];
  test (dune_project (implicit_transitive_deps (equals `True)));
  [%expect {| (dune_project (implicit_transitive_deps (equals true))) |}];
  test
    (dune_project (implicit_transitive_deps (equals `False_if_hidden_includes_supported)));
  [%expect
    {|
    (dune_project
     (implicit_transitive_deps (equals false-if-hidden-includes-supported)))
    |}];
  test
    (dunolint
       (dunolint_lang_version (gte (Dunolint0.Dunolint_lang_version.create (1, 0)))));
  [%expect {| (dunolint (dunolint_lang_version (>= 1.0))) |}];
  ()
;;
