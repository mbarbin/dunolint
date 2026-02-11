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

open! Import

let raise_config_not_applicable_err ~(path : Relative_path.t) ~location =
  (* We only expect code paths in which the path linted are located in the
     subtree of the configs accumulated during discovery. That should be an
     internal error that warrants a bug report. *)
  Err.raise
    ~loc:(Loc.of_file ~path:(path :> Fpath.t))
    ~exit_code:Err.Exit_code.internal_error
    [ Pp.text "Path is not within config location."
    ; Err.sexp [%sexp { path : Relative_path.t; location : Relative_path.t }]
    ] [@coverage off]
;;

let should_skip_subtree ~context ~(path : Relative_path.t) =
  List.exists (Dunolint_engine.Context.configs context) ~f:(fun { config; location } ->
    match Relative_path.chop_prefix path ~prefix:location with
    | None -> raise_config_not_applicable_err ~path ~location [@coverage off]
    | Some path ->
      (match Dunolint.Config.Private.view config with
       | `v1 v1 ->
         let skip_paths = Dunolint.Config.V1.skip_paths v1 |> List.concat in
         Path_in_workspace.paths_to_check_for_skip_predicates ~path
         |> List.exists ~f:(fun path ->
           let path = Relative_path.to_string path in
           List.exists skip_paths ~f:(fun glob -> Dunolint.Glob.test glob path))))
;;

let default_dune_lang_version = lazy (Dune_project.Dune_lang_version.create (2, 0))

let enclosing_dune_lang_version ~context ~(path : Relative_path.t) =
  match Dunolint_engine.Context.enclosing_dune_lang_version context with
  | None -> Some (Lazy.force default_dune_lang_version)
  | Some (Ok version_or_missing) -> version_or_missing
  | Some (Error Invalid_dune_project) ->
    (* Formatting is not applied when we do not know which version to use. We
       emit only a debug message in this case because a warning or error has
       already been emitted informing that the dune-project is invalid. Besides,
       if we are in debug mode, the actual error for that specific file has
       already been printed during the loading of the dune-project file. *)
    Err.debug
      ~loc:(Loc.of_file ~path:(path :> Fpath.t))
      (lazy
        Pp.O.
          [ Pp.text "Formatting file "
            ++ Pp_tty.path (module Relative_path) path
            ++ Pp.text " was disabled due to existing errors in enclosing "
            ++ Pp_tty.path (module String) "dune-project"
            ++ Pp.text " file."
          ]);
    None
;;

let autoformat_dune_file ~context ~path ~previous_contents ~new_contents =
  (* For the time being we are using here a heuristic to drive whether to
     autoformat linted files. This is motivated by pragmatic reasoning and lower
     friction for onboarding in various situation where formatting may or may
     not be used in projects. *)
  if String.equal previous_contents new_contents
  then new_contents
  else (
    match enclosing_dune_lang_version ~context ~path with
    | None -> new_contents
    | Some dune_lang_version ->
      let was_originally_well_formatted =
        try
          let formatted =
            Dunolint_engine.format_dune_file
              ~dune_lang_version
              ~new_contents:previous_contents
          in
          String.equal formatted previous_contents
        with
        | exn ->
          Err.debug
            ~loc:(Loc.of_file ~path:(path :> Fpath.t))
            (lazy
              [ Pp.text "Exception raised when formatting original file contents:"
              ; Err.exn exn
              ]
              [@coverage off]);
          false
      in
      if was_originally_well_formatted
      then Dunolint_engine.format_dune_file ~dune_lang_version ~new_contents
      else new_contents)
;;

let lint_stanza ~path ~context ~stanza =
  let loc =
    Sexps_rewriter.loc
      (Dunolinter.sexps_rewriter stanza)
      (Dunolinter.original_sexp stanza)
  in
  Dunolinter.Handler.emit_error_and_resume () ~loc ~f:(fun () ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
    | T { eval; enforce } ->
      List.iter (Dunolint_engine.Context.configs context) ~f:(fun { config; location } ->
        match Relative_path.chop_prefix path ~prefix:location with
        | None -> raise_config_not_applicable_err ~path ~location [@coverage off]
        | Some path ->
          List.iter
            (match Dunolint.Config.Private.view config with
             | `v1 v1 -> Dunolint.Config.V1.rules v1)
            ~f:(fun rule ->
              match
                Dunolint.Rule.eval rule ~f:(fun predicate -> eval ~path ~predicate)
              with
              | `return -> ()
              | `enforce condition -> enforce ~path ~condition)))
;;

let lint_file
      (module Linter : Dunolinter.S)
      ~dunolint_engine
      ~context
      ~(path : Relative_path.t)
  =
  let previous_contents_ref = ref "" in
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
        Linter.visit linter ~f:(fun stanza -> lint_stanza ~path ~context ~stanza);
        Linter.contents linter)
    ~autoformat_file:(fun ~new_contents ->
      let previous_contents = !previous_contents_ref in
      autoformat_dune_file ~context ~path ~previous_contents ~new_contents)
;;

let should_skip_file ~context ~path =
  List.exists (Dunolint_engine.Context.configs context) ~f:(fun { config; location } ->
    match Relative_path.chop_prefix path ~prefix:location with
    | None -> raise_config_not_applicable_err ~path ~location [@coverage off]
    | Some path ->
      (match Dunolint.Config.Private.view config with
       | `v1 v1 ->
         let filename = Relative_path.to_string path in
         let skip_files = Dunolint.Config.V1.skip_paths v1 |> List.concat in
         List.exists skip_files ~f:(fun glob -> Dunolint.Glob.test glob filename)))
;;

let visit_directory ~dunolint_engine ~context ~parent_dir ~files =
  match should_skip_subtree ~context ~path:parent_dir with
  | true -> Dunolint_engine.Visitor_decision.Skip_subtree
  | false ->
    List.iter files ~f:(fun file ->
      match Dunolint.Linted_file_kind.of_string file with
      | Error (`Msg _) -> ()
      | Ok linted_file_kind ->
        let path = Relative_path.extend parent_dir (Fsegment.v file) in
        if not (should_skip_file ~context ~path)
        then (
          let lint_file m = lint_file m ~dunolint_engine ~context ~path in
          match linted_file_kind with
          | `dune -> lint_file (module Dune_linter)
          | `dune_project -> lint_file (module Dune_project_linter)
          | `dune_workspace -> lint_file (module Dune_workspace_linter)
          | `dunolint -> lint_file (module Dunolint_linter)));
    Dunolint_engine.Visitor_decision.Continue
;;
