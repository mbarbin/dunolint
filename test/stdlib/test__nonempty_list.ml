(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "to_list" =
  print_dyn (Dyn.list Dyn.int (Nonempty_list.to_list [ 1 ]));
  [%expect {| [ 1 ] |}];
  print_dyn (Dyn.list Dyn.int (Nonempty_list.to_list [ 1; 2; 3 ]));
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "iter" =
  Nonempty_list.iter [ 1; 2; 3 ] ~f:(fun i -> print_dyn (Dyn.int i));
  [%expect
    {|
    1
    2
    3
    |}];
  ()
;;

let%expect_test "for_all" =
  require (Nonempty_list.for_all [ 2; 4 ] ~f:(fun i -> i mod 2 = 0));
  require (not (Nonempty_list.for_all [ 2; 3 ] ~f:(fun i -> i mod 2 = 0)));
  [%expect {||}];
  ()
;;
