(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "phys_equal" =
  let a () = `a ("a" ^ "b") in
  let a1 = a () in
  require (phys_equal a1 a1);
  [%expect {||}];
  let a2 = a () in
  require (not (phys_equal a1 a2));
  [%expect {||}];
  require (Stdlib.( = ) a1 a2);
  [%expect {||}];
  ()
;;
