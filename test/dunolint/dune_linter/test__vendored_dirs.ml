(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* The implementation is shared with [data_only_dirs] (see [Directory_list]), which is
   tested in more details. *)

let%expect_test "rewrite" =
  let (sexps_rewriter, field), t =
    Test_helpers.parse
      (module Dune_linter.Vendored_dirs)
      ~path:(Fpath.v "dune")
      {|(vendored_dirs
 zarith
 base ; Pinned.
 ocamlformat)|}
  in
  Dune_linter.Vendored_dirs.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect
    {|
    (vendored_dirs
     base ; Pinned.
     ocamlformat
     zarith)
    |}];
  ()
;;
