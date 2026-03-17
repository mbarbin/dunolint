(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "sexp round-trip" =
  let test i =
    let sexp = Int.sexp_of_t i in
    let j = Int.t_of_sexp sexp in
    print_dyn (Dyn.int j)
  in
  test 0;
  [%expect {| 0 |}];
  test 42;
  [%expect {| 42 |}];
  test (-1);
  [%expect {| -1 |}];
  ()
;;

let%expect_test "t_of_sexp - invalid" =
  require_does_raise (fun () -> Int.t_of_sexp (Sexp.Atom "abc"));
  [%expect
    {| (Of_sexp_error "int_of_sexp: (Failure int_of_string)" (invalid_sexp abc)) |}];
  require_does_raise (fun () -> Int.t_of_sexp (Sexp.List []));
  [%expect {| (Of_sexp_error "int_of_sexp: atom needed" (invalid_sexp ())) |}];
  ()
;;
