(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp (Dune.Pp.Name.of_string str : (Dune.Pp.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Pp_name")) |}];
  test "pp";
  [%expect {| (Ok pp) |}];
  test "pp-dash";
  [%expect {| (Ok pp-dash) |}];
  test "pp_underscore";
  [%expect {| (Ok pp_underscore) |}];
  test "pp.dot";
  [%expect {| (Ok pp.dot) |}];
  test "pp#sharp";
  [%expect {| (Error (Msg "\"pp#sharp\": invalid Pp_name")) |}];
  test "pp@at";
  [%expect {| (Error (Msg "\"pp@at\": invalid Pp_name")) |}];
  ()
;;

let%expect_test "compare" =
  let test vs =
    let vs = List.map vs ~f:Dune.Pp.Name.v in
    print_s [%sexp (List.sort vs ~compare:Dune.Pp.Name.compare : Dune.Pp.Name.t list)]
  in
  test [ "a"; "b" ];
  [%expect {| (a b) |}];
  test [ "ppx_this"; "that"; "ppx_that"; "lib.ppx_bcd"; "otherlib.ppx_abc" ];
  [%expect {| (otherlib.ppx_abc lib.ppx_bcd ppx_that ppx_this that) |}];
  ()
;;
