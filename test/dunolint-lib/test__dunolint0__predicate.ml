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
  let v1_0 = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let v1_5 = Dunolint0.Dunolint_lang_version.create (1, 5) in
  let p1 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_0)) in
  let p2 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_0)) in
  let p3 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_5)) in
  (* Same value, different references. *)
  print_s [%sexp (Dunolint0.Predicate.equal p1 p2 : bool)];
  [%expect {| true |}];
  (* Physical equality. *)
  print_s [%sexp (Dunolint0.Predicate.equal p1 p1 : bool)];
  [%expect {| true |}];
  (* Different values. *)
  print_s [%sexp (Dunolint0.Predicate.equal p1 p3 : bool)];
  [%expect {| false |}];
  ()
;;

let%expect_test "compare" =
  let v1_0 = Dunolint0.Dunolint_lang_version.create (1, 0) in
  let v1_5 = Dunolint0.Dunolint_lang_version.create (1, 5) in
  let p1 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_0)) in
  let p2 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_0)) in
  let p3 : Dunolint0.Predicate.t = `dunolint_lang_version (Blang.base (`gte v1_5)) in
  (* Same value, different references. *)
  print_s [%sexp (Dunolint0.Predicate.compare p1 p2 : int)];
  [%expect {| 0 |}];
  (* Physical equality. *)
  print_s [%sexp (Dunolint0.Predicate.compare p1 p1 : int)];
  [%expect {| 0 |}];
  (* Different values - p1 < p3 because v1_0 < v1_5. *)
  print_s [%sexp (Dunolint0.Predicate.compare p1 p3 < 0 : bool)];
  [%expect {| true |}];
  print_s [%sexp (Dunolint0.Predicate.compare p3 p1 > 0 : bool)];
  [%expect {| true |}];
  ()
;;
