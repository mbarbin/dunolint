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

let eval_path ~path ~predicate =
  match (predicate : Dunolint.Predicate.t) with
  | `dune _ | `dune_project _ -> Dunolint.Trilang.Undefined
  | `path condition ->
    Blang.eval condition (function
      | `equals value -> Relative_path.equal path value
      | `glob glob -> Dunolint.Glob.test glob (Relative_path.to_string path))
    |> Dunolint.Trilang.const
;;

let maybe_autoformat_file ~previous_contents ~new_contents =
  (* For the time being we are using here a heuristic to drive whether to
     autoformat linted files. This is motivated by pragmatic reasoning and lower
     friction for onboarding in various situation where formatting may or may
     not be used in projects. *)
  if String.equal previous_contents new_contents
  then new_contents
  else (
    let was_originally_well_formatted =
      try
        let formatted =
          Dunolint_engine.format_dune_file ~new_contents:previous_contents
        in
        String.equal formatted previous_contents
      with
      | _ -> false
    in
    if was_originally_well_formatted
    then Dunolint_engine.format_dune_file ~new_contents
    else new_contents)
;;

module Lint_file (Linter : Dunolinter.S) = struct
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

  let lint_file ~dunolint_engine ~rules ~(path : Relative_path.t) =
    let previous_contents_ref = ref "" in
    let visitor_decision = ref Dunolint_engine.Visitor_decision.Continue in
    Dunolint_engine.lint_file
      dunolint_engine
      ~path
      ?create_file:None
      ~rewrite_file:(fun ~previous_contents ->
        previous_contents_ref := previous_contents;
        match Linter.create ~path ~original_contents:previous_contents with
        | Error { loc; message } ->
          Err.error ~loc [ Pp.textf "%s" message ];
          previous_contents
        | Ok linter ->
          let () =
            try Linter.visit linter ~f:(fun stanza -> lint_stanza ~rules ~stanza) with
            | Skip_subtree -> visitor_decision := Skip_subtree
          in
          Linter.contents linter)
      ~autoformat_file:(fun ~new_contents ->
        let previous_contents = !previous_contents_ref in
        maybe_autoformat_file ~previous_contents ~new_contents);
    !visitor_decision
  ;;
end

module Dune_lint = Lint_file (Dune_linter)
module Dune_project_lint = Lint_file (Dune_project_linter)

let visit_directory ~dunolint_engine ~config ~parent_dir ~files =
  Log.debug (fun () ->
    Pp.O.
      [ Pp.text "Dunolint visit_directory "
        ++ Pp_tty.path (module Relative_path) parent_dir
      ]);
  match
    match Dunolint.Config.skip_subtree config with
    | None -> `return
    | Some condition ->
      Dunolint.Rule.eval condition ~f:(fun predicate ->
        eval_path ~path:parent_dir ~predicate:(predicate :> Dunolint.Predicate.t))
  with
  | `enforce nothing -> Nothing.unreachable_code nothing
  | `skip_subtree ->
    Log.debug (fun () ->
      Pp.O.
        [ Pp.text "Skipping directory " ++ Pp_tty.path (module Relative_path) parent_dir ]);
    Dunolint_engine.Visitor_decision.Skip_subtree
  | `return ->
    let rules = Dunolint.Config.rules config in
    let rec loop = function
      | [] -> Dunolint_engine.Visitor_decision.Continue
      | file :: files ->
        let path = Relative_path.extend parent_dir (Fsegment.v file) in
        (match
           match file with
           | "dune" -> Dune_lint.lint_file ~dunolint_engine ~rules ~path
           | "dune-project" -> Dune_project_lint.lint_file ~dunolint_engine ~rules ~path
           | _ -> Dunolint_engine.Visitor_decision.Continue
         with
         | Dunolint_engine.Visitor_decision.Continue -> loop files
         | (Break | Skip_subtree) as visitor_decision -> visitor_decision)
    in
    loop files
;;

let main =
  Command.make
    ~summary:"lint project"
    (let%map_open.Command dunolint_engine_config = Dunolint_engine.Config.arg
     and () = Log_cli.set_config ()
     and config =
       Arg.named_opt [ "config" ] Param.file ~doc:"Path to dunolint config file"
     and below = Common.below ~doc:"Lint only below this path"
     and enforce =
       Arg.named_multi
         [ "enforce" ]
         (Common.sexpable_param (module Dunolint.Condition))
         ~docv:"COND"
         ~doc:"Add condition to enforce"
       >>| List.map ~f:(fun condition -> `enforce condition)
     in
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
     Dunolint_engine.run ~config:dunolint_engine_config
     @@ fun dunolint_engine ->
     Dunolint_engine.visit
       dunolint_engine
       ?below
       ~f:(fun ~parent_dir ~subdirectories:_ ~files ->
         visit_directory ~dunolint_engine ~config ~parent_dir ~files))
;;
