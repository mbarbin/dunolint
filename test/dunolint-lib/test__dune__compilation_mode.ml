(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "all" =
  List.iter Dune.Compilation_mode.all ~f:(fun compilation_mode ->
    print_s (compilation_mode |> Dune.Compilation_mode.sexp_of_t));
  [%expect
    {|
    byte
    native
    best
    melange
    |}];
  ()
;;
