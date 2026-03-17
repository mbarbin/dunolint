(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
