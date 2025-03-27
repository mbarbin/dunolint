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

let skip_subtree ~config ~path =
  match Dunolint.Config.skip_subtree config with
  | None -> `return
  | Some condition ->
    (match
       Dunolint.Rule.eval condition ~f:(fun predicate ->
         Dunolinter.eval_path ~path ~predicate:(predicate :> Dunolint.Predicate.t))
     with
     | `enforce nothing -> Nothing.unreachable_code nothing [@coverage off]
     | (`return | `skip_subtree) as result -> result)
;;

exception Skip_subtree

let lint_stanza ~rules ~stanza =
  let loc =
    Sexps_rewriter.loc
      (Dunolinter.sexps_rewriter stanza)
      (Dunolinter.original_sexp stanza)
  in
  Dunolinter.Handler.emit_error_and_resume () ~loc ~f:(fun () ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
    | T { eval; enforce } ->
      List.iter rules ~f:(fun rule ->
        match Dunolint.Rule.eval rule ~f:eval with
        | `return -> ()
        | `enforce condition -> enforce condition
        | `skip_subtree -> raise Skip_subtree))
;;

let lint_file (module Linter : Dunolinter.S) ~format_file ~rules ~path ~original_contents =
  match Linter.create ~path ~original_contents with
  | Error { loc; message } -> Err.raise ~loc [ Pp.textf "%s" message ]
  | Ok linter ->
    let () =
      try Linter.visit linter ~f:(fun stanza -> lint_stanza ~rules ~stanza) with
      | Skip_subtree -> ()
    in
    let new_contents = Linter.contents linter in
    if format_file then Dunolint_engine.format_dune_file ~new_contents else new_contents
;;

let select_linter ~path =
  let filename = Fpath.filename path in
  match Dunolint.Linted_file_kind.of_string filename with
  | Ok linted_file_kind ->
    (match linted_file_kind with
     | `dune -> (module Dune_linter : Dunolinter.S)
     | `dune_project -> (module Dune_project_linter : Dunolinter.S))
  | Error (`Msg _msg) ->
    Err.raise
      Pp.O.
        [ Pp.text "Cannot infer the file kind from the filename "
          ++ Pp_tty.path (module String) filename
          ++ Pp.verbatim "."
        ]
      ~hints:
        Pp.O.
          [ Pp.text "You may override the filename with the flag "
            ++ Pp_tty.kwd (module String) "--filename"
            ++ Pp.verbatim "."
          ]
;;

let select_path ~cwd ~filename ~file =
  match Option.first_some filename file with
  | None -> Relative_path.v "stdin"
  | Some file ->
    (match Fpath.classify file with
     | `Relative path -> path
     | `Absolute path ->
       (match Absolute_path.chop_prefix path ~prefix:cwd with
        | Some path -> path
        | None ->
          Err.raise
            Pp.O.
              [ Pp.text "Invalid absolute file path, must be within cwd."
              ; Pp.verbatim "file: " ++ Pp_tty.path (module Absolute_path) path
              ; Pp.verbatim "cwd: " ++ Pp_tty.path (module Absolute_path) cwd
              ]))
;;

let main =
  Command.make
    ~summary:"Lint a single file."
    ~readme:(fun () ->
      {|
This command is meant to ease the integration with editors in workflows that enable linting on save.

This will read the contents of a build file either from $(b,disk) or from $(b,stdin) and print its linted result on $(b,stdout).

By default, the contents will be read from $(b,stdin). You may supply the path to a file instead.

When the contents of the file is read from stdin, or if the file given does not permit to recognize the linted file kind solely from its path, the name of the file may be overridden.
|})
    (let%map_open.Command () = Log_cli.set_config ()
     and file =
       Arg.pos_opt
         ~pos:0
         (Param.validated_string (module Fpath))
         ~docv:"FILE"
         ~doc:"Path to file to lint (by default reads from stdin)."
     and filename =
       Arg.named_opt
         [ "filename" ]
         (Param.validated_string (module Fpath))
         ~docv:"path/to/file"
         ~doc:
           "When supplied, this value is only used as a string in error messages and to \
            derive the linted file kind from its basename, but that path is not used to \
            load contents from disk. This may be used to override an actual file name or \
            to name the input when it comes from $(b,stdin)."
     and config =
       Arg.named_opt [ "config" ] Param.file ~doc:"Path to dunolint config file."
     and format_file =
       Arg.named_with_default
         [ "format-file" ]
         Param.bool
         ~default:true
         ~doc:"Format file with after linting, using [dune format-dune-file]."
     and enforce =
       Arg.named_multi
         [ "enforce" ]
         (Common.sexpable_param (module Dunolint.Condition))
         ~docv:"COND"
         ~doc:"Add condition to enforce."
       >>| List.map ~f:(fun condition -> `enforce condition)
     in
     let cwd = Unix.getcwd () |> Absolute_path.v in
     let config =
       match config with
       | Some config ->
         let contents = In_channel.read_all config in
         Parsexp.Conv_single.parse_string_exn contents Dunolint.Config.t_of_sexp
       | None ->
         Dunolint.Config.create ~skip_subtree:(Common.skip_subtree ~globs:[]) ~rules:[] ()
     in
     let config =
       Dunolint.Config.create
         ?skip_subtree:(Dunolint.Config.skip_subtree config)
         ~rules:(Dunolint.Config.rules config @ enforce)
         ()
     in
     let path = select_path ~cwd ~filename ~file in
     let linter = select_linter ~path:(path :> Fpath.t) in
     let original_contents =
       match file with
       | Some file -> In_channel.read_all (file |> Fpath.to_string)
       | None -> In_channel.input_all In_channel.stdin
     in
     let output =
       match skip_subtree ~config ~path with
       | `skip_subtree -> original_contents
       | `return ->
         let rules = Dunolint.Config.rules config in
         lint_file linter ~format_file ~rules ~path ~original_contents
     in
     print_string output)
;;
