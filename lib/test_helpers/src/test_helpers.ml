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

let read_sexp_field ~path original_contents =
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok r -> r
    | Error { loc; message } -> Err.raise ~loc [ Pp.text message ] [@coverage off]
  in
  match Sexps_rewriter.original_sexps sexps_rewriter with
  | [ field ] -> sexps_rewriter, field
  | sexps -> Err.raise [ Pp.textf "Expected exactly 1 sexp, got %d." (List.length sexps) ]
;;

let parse
      (type a)
      (module M : Dunolinter.Sexp_handler.S with type t = a)
      ~path
      original_contents
  =
  let sexps_rewriter, field = read_sexp_field ~path original_contents in
  let t =
    try M.read ~sexps_rewriter ~field with
    | Sexp.Of_sexp_error (_, sexp) ->
      (* We redact the message because it is contains paths to source files, which
         makes it inconvenient when relocating the code in sub repos. *)
      raise_s
        [%sexp Of_sexp_error, ("_", { invalid_sexp = (sexp : Sexp.t) })] [@coverage off]
  in
  (sexps_rewriter, field), t
;;

let is_true b = require_equal [%here] (module Dunolint.Trilang) b True
let is_false b = require_equal [%here] (module Dunolint.Trilang) b False
let is_undefined b = require_equal [%here] (module Dunolint.Trilang) b Undefined

let run_linter ~config =
  let dunolint_engine =
    Dunolint_engine.create ~root_configs:[ config ] ~running_mode:Dry_run ()
  in
  let () =
    Dunolint_engine.visit
      dunolint_engine
      ~f:(fun ~context ~parent_dir ~subdirectories:_ ~files ->
        Dunolint_cli.Private.Linter.visit_directory
          ~dunolint_engine
          ~context
          ~parent_dir
          ~files)
  in
  Dunolint_engine.materialize dunolint_engine
;;
