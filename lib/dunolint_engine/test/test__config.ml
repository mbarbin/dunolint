(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

let eval_args args =
  let command =
    Cmdlang.Command.make Dunolint_engine.Config.arg ~summary:"Test eval-stdlib-runner."
  in
  match Cmdlang_stdlib_runner.eval command ~argv:(Array.of_list ("dunolint" :: args)) with
  | Ok t -> print_s [%sexp (t : Dunolint_engine.Config.t)]
  | Error (`Help msg) -> print_endline msg [@coverage off]
  | Error (`Bad msg) ->
    (Stdlib.print_string msg;
     print_s [%sexp "Evaluation Failed", { exit_code = (2 : int) }])
    [@coverage off]
  | exception e -> print_s [%sexp "Evaluation Raised", (e : Exn.t)]
;;

let%expect_test "running modes" =
  (* By default, when not running in an interactive terminal, the mode is to
     apply all lints without asking for confirmation.

     This is indeed what happens during the expect-test, because it is run in a
     context where stdout is not a tty. *)
  eval_args [];
  [%expect {| ((running_mode Force_yes)) |}];
  (* The interactive mode can be forced. *)
  eval_args [ "--interactive" ];
  [%expect {| ((running_mode Interactive)) |}];
  (* There are other running modes available. *)
  eval_args [ "--check" ];
  [%expect {| ((running_mode Check)) |}];
  eval_args [ "--dry-run" ];
  [%expect {| ((running_mode Dry_run)) |}];
  (* But they are mutually exclusive. *)
  eval_args [ "--check"; "--dry-run" ];
  [%expect
    {|
    ("Evaluation Raised"
     "Conflicting flags [dry-run], [check]. Please choose one.")
    |}];
  ()
;;

let%expect_test "create" =
  (* The API lets you create config programmatically. *)
  let t = Dunolint_engine.Config.create ~running_mode:Dry_run in
  print_s [%sexp (t : Dunolint_engine.Config.t)];
  [%expect {| ((running_mode Dry_run)) |}];
  ()
;;
