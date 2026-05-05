(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let recognize = Dunolint_cli.Private.Recognize_linted_file.recognize
let kind_to_dyn kind = Dyn.string (Dunolint.Linted_file_kind.to_string kind)

let%expect_test "recognize" =
  List.iter Dunolint.Linted_file_kind.all ~f:(fun linted_file_kind ->
    let str = Dunolint.Linted_file_kind.to_string linted_file_kind in
    let kind =
      match recognize (Fsegment.v str) with
      | Some kind -> kind
      | None -> assert false
    in
    require_equal (module Dunolint.Linted_file_kind) linted_file_kind kind;
    print_endline str);
  [%expect
    {|
    dune
    dune-project
    dune-workspace
    dunolint
    |}];
  let test str =
    print_dyn
      (Dyn.pair Dyn.string (Dyn.option kind_to_dyn) (str, recognize (Fsegment.v str)))
  in
  (* Custom dune-workspace files prefixed with [dune-workspace.] are
     recognized as [dune_workspace]. *)
  test "dune-workspace.ci";
  test "dune-workspace.5.3";
  test "dune-workspace.5.4";
  [%expect
    {|
    ("dune-workspace.ci", Some "dune-workspace")
    ("dune-workspace.5.3", Some "dune-workspace")
    ("dune-workspace.5.4", Some "dune-workspace")
    |}];
  (* Names that merely look like the prefix without a [.] separator are not
     recognized. *)
  test "dune-workspace-foo";
  test "invalid";
  [%expect
    {|
    ("dune-workspace-foo", None)
    ("invalid", None)
    |}];
  ()
;;
