(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "to_dyn - root" =
  print_dyn (Absolute_path.to_dyn Absolute_path.root);
  [%expect {| "/" |}];
  ()
;;

let%expect_test "to_dyn - file path" =
  print_dyn (Absolute_path.to_dyn (Absolute_path.v "/foo/bar"));
  [%expect {| "/foo/bar" |}];
  ()
;;

let%expect_test "to_dyn - directory path" =
  print_dyn (Absolute_path.to_dyn (Absolute_path.v "/foo/bar/"));
  [%expect {| "/foo/bar/" |}];
  ()
;;
