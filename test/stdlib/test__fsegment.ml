(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "to_dyn - simple segment" =
  print_dyn (Fsegment.to_dyn (Fsegment.v "foo"));
  [%expect {| "foo" |}];
  ()
;;

let%expect_test "to_dyn - dot" =
  print_dyn (Fsegment.to_dyn Fsegment.dot);
  [%expect {| "." |}];
  ()
;;

let%expect_test "to_dyn - dot_dot" =
  print_dyn (Fsegment.to_dyn Fsegment.dot_dot);
  [%expect {| ".." |}];
  ()
;;
