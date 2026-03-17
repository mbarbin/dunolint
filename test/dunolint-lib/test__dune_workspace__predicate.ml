(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
