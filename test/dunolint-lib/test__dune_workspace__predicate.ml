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

let%expect_test "equal" =
  let v3_0 = Dune_workspace.Dune_lang_version.create (3, 0) in
  let v3_17 = Dune_workspace.Dune_lang_version.create (3, 17) in
  let p1 : Dune_workspace.Predicate.t = `dune_lang_version (Blang.base (`gte v3_0)) in
  let p2 : Dune_workspace.Predicate.t = `dune_lang_version (Blang.base (`gte v3_0)) in
  let p3 : Dune_workspace.Predicate.t = `dune_lang_version (Blang.base (`gte v3_17)) in
  (* Same value, different references. *)
  print_s [%sexp (Dune_workspace.Predicate.equal p1 p2 : bool)];
  [%expect {| true |}];
  (* Physical equality. *)
  print_s [%sexp (Dune_workspace.Predicate.equal p1 p1 : bool)];
  [%expect {| true |}];
  (* Different values. *)
  print_s [%sexp (Dune_workspace.Predicate.equal p1 p3 : bool)];
  [%expect {| false |}];
  ()
;;
