(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp
        (Dune.Package.Name.of_string str
         : (Dune.Package.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Package_name")) |}];
  test "pkg";
  [%expect {| (Ok pkg) |}];
  test "pkg-dash";
  [%expect {| (Ok pkg-dash) |}];
  test "pkg_underscore";
  [%expect {| (Ok pkg_underscore) |}];
  test "pkg_UPPERCASE";
  [%expect {| (Ok pkg_UPPERCASE) |}];
  test "pkg.dot";
  [%expect {| (Error (Msg "\"pkg.dot\": invalid Package_name")) |}];
  test "pkg#sharp";
  [%expect {| (Error (Msg "\"pkg#sharp\": invalid Package_name")) |}];
  test "pkg@at";
  [%expect {| (Error (Msg "\"pkg@at\": invalid Package_name")) |}];
  ()
;;
