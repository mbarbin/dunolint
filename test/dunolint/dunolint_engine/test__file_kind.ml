(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let%expect_test "to_string" =
  List.iter Dunolint_engine.File_kind.all ~f:(fun t ->
    print_endline (Dunolint_engine.File_kind.to_string t));
  [%expect
    {|
    Regular file
    Directory
    Character device
    Block device
    Symbolic link
    Named pipe
    Socket
    |}];
  ()
;;
