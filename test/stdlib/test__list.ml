(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

let%expect_test "count - empty list" =
  print_dyn (Dyn.int (List.count [] ~f:(fun _ -> (assert false [@coverage off]))));
  [%expect {| 0 |}];
  ()
;;

let%expect_test "count - no match" =
  print_dyn (Dyn.int (List.count [ 1; 2; 3 ] ~f:(fun x -> x > 10)));
  [%expect {| 0 |}];
  ()
;;

let%expect_test "count - all match" =
  print_dyn (Dyn.int (List.count [ 1; 2; 3 ] ~f:(fun x -> x > 0)));
  [%expect {| 3 |}];
  ()
;;

let%expect_test "count - partial match" =
  print_dyn (Dyn.int (List.count [ 1; 2; 3; 4; 5 ] ~f:(fun x -> x mod 2 = 0)));
  [%expect {| 2 |}];
  ()
;;

let%expect_test "count - singleton" =
  print_dyn (Dyn.int (List.count [ 42 ] ~f:(fun x -> x = 42)));
  [%expect {| 1 |}];
  ()
;;

let%expect_test "dedup_and_sort - empty list" =
  print_dyn (Dyn.list Dyn.int (List.dedup_and_sort [] ~compare:Int.compare));
  [%expect {| [] |}];
  ()
;;

let%expect_test "dedup_and_sort - singleton" =
  print_dyn (Dyn.list Dyn.int (List.dedup_and_sort [ 1 ] ~compare:Int.compare));
  [%expect {| [ 1 ] |}];
  ()
;;

let%expect_test "dedup_and_sort - already sorted, no duplicates" =
  print_dyn (Dyn.list Dyn.int (List.dedup_and_sort [ 1; 2; 3 ] ~compare:Int.compare));
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "dedup_and_sort - unsorted with duplicates" =
  print_dyn
    (Dyn.list Dyn.int (List.dedup_and_sort [ 3; 1; 2; 1; 3; 2 ] ~compare:Int.compare));
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "dedup_and_sort - all duplicates" =
  print_dyn (Dyn.list Dyn.int (List.dedup_and_sort [ 5; 5; 5; 5 ] ~compare:Int.compare));
  [%expect {| [ 5 ] |}];
  ()
;;

let%expect_test "dedup_and_sort - reverse sorted" =
  print_dyn
    (Dyn.list Dyn.int (List.dedup_and_sort [ 5; 4; 3; 2; 1 ] ~compare:Int.compare));
  [%expect {| [ 1; 2; 3; 4; 5 ] |}];
  ()
;;

let%expect_test "find - empty list" =
  print_dyn
    (Dyn.option Dyn.int (List.find [] ~f:(fun _ -> (assert false [@coverage off]))));
  [%expect {| None |}];
  ()
;;

let%expect_test "find - found" =
  print_dyn (Dyn.option Dyn.int (List.find [ 1; 2; 3 ] ~f:(fun x -> x = 2)));
  [%expect {| Some 2 |}];
  ()
;;

let%expect_test "find - not found" =
  print_dyn (Dyn.option Dyn.int (List.find [ 1; 2; 3 ] ~f:(fun x -> x = 42)));
  [%expect {| None |}];
  ()
;;

let%expect_test "find - returns first match" =
  print_dyn (Dyn.option Dyn.int (List.find [ 1; 2; 3; 4 ] ~f:(fun x -> x mod 2 = 0)));
  [%expect {| Some 2 |}];
  ()
;;

let%expect_test "last_exn - singleton" =
  print_dyn (Dyn.int (List.last_exn [ 1 ]));
  [%expect {| 1 |}];
  ()
;;

let%expect_test "last_exn - multiple elements" =
  print_dyn (Dyn.int (List.last_exn [ 1; 2; 3 ]));
  [%expect {| 3 |}];
  ()
;;

let%expect_test "last_exn - empty list raises" =
  require_does_raise (fun () -> List.last_exn []);
  [%expect {| (Failure List.last_exn) |}];
  ()
;;

let%expect_test "max_elt - empty list" =
  print_dyn (Dyn.option Dyn.int (List.max_elt [] ~compare:Int.compare));
  [%expect {| None |}];
  ()
;;

let%expect_test "max_elt - singleton" =
  print_dyn (Dyn.option Dyn.int (List.max_elt [ 42 ] ~compare:Int.compare));
  [%expect {| Some 42 |}];
  ()
;;

let%expect_test "max_elt - multiple elements" =
  print_dyn (Dyn.option Dyn.int (List.max_elt [ 3; 1; 4; 1; 5; 9 ] ~compare:Int.compare));
  [%expect {| Some 9 |}];
  ()
;;

let%expect_test "max_elt - all equal" =
  print_dyn (Dyn.option Dyn.int (List.max_elt [ 7; 7; 7 ] ~compare:Int.compare));
  [%expect {| Some 7 |}];
  ()
;;

let%expect_test "max_elt - max at beginning" =
  print_dyn (Dyn.option Dyn.int (List.max_elt [ 9; 1; 2 ] ~compare:Int.compare));
  [%expect {| Some 9 |}];
  ()
;;

let%expect_test "sum - empty list" =
  print_dyn (Dyn.int (List.sum (module Int) [] ~f:Fun.id));
  [%expect {| 0 |}];
  ()
;;

let%expect_test "sum - singleton" =
  print_dyn (Dyn.int (List.sum (module Int) [ 5 ] ~f:Fun.id));
  [%expect {| 5 |}];
  ()
;;

let%expect_test "sum - multiple elements" =
  print_dyn (Dyn.int (List.sum (module Int) [ 1; 2; 3; 4 ] ~f:Fun.id));
  [%expect {| 10 |}];
  ()
;;

let%expect_test "sum - with projection" =
  print_dyn (Dyn.int (List.sum (module Int) [ "a"; "bb"; "ccc" ] ~f:String.length));
  [%expect {| 6 |}];
  ()
;;
