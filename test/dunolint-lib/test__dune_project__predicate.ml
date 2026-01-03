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

open Dunolint.Std

let%expect_test "equal" =
  let equal = Dune_project.Predicate.equal in
  let dune_lang_a = `dune_lang_version Blang.true_ in
  let generate_a = `generate_opam_files (Blang.base `is_present) in
  let implicit_a = `implicit_transitive_deps (Blang.base (`equals `True)) in
  let implicit_b = `implicit_transitive_deps (Blang.base (`equals `False)) in
  let name_a = `name (Blang.base (`equals (Dune_project.Name.v "my-proj"))) in
  (* Physical equality. *)
  require (equal implicit_a implicit_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`dune_lang_version Blang.true_) (`dune_lang_version Blang.true_));
  [%expect {||}];
  require
    (equal
       (`generate_opam_files (Blang.base `is_present))
       (`generate_opam_files (Blang.base `is_present)));
  [%expect {||}];
  require
    (equal
       (`implicit_transitive_deps (Blang.base (`equals `True)))
       (`implicit_transitive_deps (Blang.base (`equals `True))));
  [%expect {||}];
  require
    (equal
       (`name (Blang.base (`equals (Dune_project.Name.v "my-proj"))))
       (`name (Blang.base (`equals (Dune_project.Name.v "my-proj")))));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal implicit_a implicit_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal dune_lang_a generate_a));
  [%expect {||}];
  require (not (equal generate_a implicit_a));
  [%expect {||}];
  require (not (equal implicit_a name_a));
  [%expect {||}];
  require (not (equal name_a dune_lang_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune_project.Predicate) p in
  test (generate_opam_files (Blang.base `is_present));
  [%expect {| (generate_opam_files is_present) |}];
  test (implicit_transitive_deps (equals `True));
  [%expect {| (implicit_transitive_deps (equals true)) |}];
  test (implicit_transitive_deps (equals `False_if_hidden_includes_supported));
  [%expect {| (implicit_transitive_deps (equals false-if-hidden-includes-supported)) |}];
  test (name (equals (Dune_project.Name.v "my-project")));
  [%expect {| (name (equals my-project)) |}];
  ()
;;
