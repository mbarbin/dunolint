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
       | `v0 v0 ->
         (match Dunolint.Config.V0.skip_subtree v0 with
          | None -> false
          | Some condition ->
            Path_in_workspace.paths_to_check_for_skip_predicates ~path
            |> List.exists ~f:(fun path ->
              match
                Dunolint.Rule.eval condition ~f:(fun (`path condition) ->
                  Dunolinter.eval_path ~path ~condition)
              with
              | `enforce _ -> .
              | `return -> false
              | `skip_subtree -> true))
       | `v1 v1 ->
         let skip_paths = Dunolint.Config.V1.skip_paths v1 |> List.concat in
         Path_in_workspace.paths_to_check_for_skip_predicates ~path
         |> List.exists ~f:(fun path ->
           let path = Relative_path.to_string path in
           List.exists skip_paths ~f:(fun glob -> Dunolint.Glob.test glob path))))
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

module Visitor_decision = struct
  (* A subtype of [Dunolint_engine.Visitor_decision] used by [Lint_file]. *)
  type t =
    | Continue
    | Skip_subtree
end

let lint_stanza ~path ~context ~stanza ~(return : _ With_return.return) =
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
          List.iter (Dunolint.Config.rules config) ~f:(fun rule ->
            match Dunolint.Rule.eval rule ~f:(fun predicate -> eval ~path ~predicate) with
            | `return -> ()
            | `enforce condition -> enforce ~path ~condition
            | `skip_subtree -> return.return `skip_subtree)))
;;

module Lint_file (Linter : Dunolinter.S) = struct
  let lint_file ~dunolint_engine ~context ~(path : Relative_path.t) =
    let previous_contents_ref = ref "" in
    let visitor_decision = ref Visitor_decision.Continue in
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
          let result =
            With_return.with_return (fun return ->
              Linter.visit linter ~f:(fun stanza ->
                lint_stanza ~path ~context ~stanza ~return);
              `continue)
          in
          let () =
            match result with
            | `continue -> ()
            | `skip_subtree -> visitor_decision := Visitor_decision.Skip_subtree
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

let should_skip_file ~context ~path =
  List.exists (Dunolint_engine.Context.configs context) ~f:(fun { config; location } ->
    match Relative_path.chop_prefix path ~prefix:location with
    | None -> raise_config_not_applicable_err ~path ~location [@coverage off]
    | Some path ->
      (match Dunolint.Config.Private.view config with
       | `v0 _ -> false
       | `v1 v1 ->
         let filename = Relative_path.to_string path in
         let skip_files = Dunolint.Config.V1.skip_paths v1 |> List.concat in
         List.exists skip_files ~f:(fun glob -> Dunolint.Glob.test glob filename)))
;;

let visit_directory ~dunolint_engine ~context ~parent_dir ~files =
  match should_skip_subtree ~context ~path:parent_dir with
  | true -> Dunolint_engine.Visitor_decision.Skip_subtree
  | false ->
    let rec loop = function
      | [] -> Dunolint_engine.Visitor_decision.Continue
      | file :: files ->
        (match
           match Dunolint.Linted_file_kind.of_string file with
           | Error (`Msg _) -> Visitor_decision.Continue
           | Ok linted_file_kind ->
             let path = Relative_path.extend parent_dir (Fsegment.v file) in
             if should_skip_file ~context ~path
             then Visitor_decision.Continue
             else (
               match linted_file_kind with
               | `dune -> Dune_lint.lint_file ~dunolint_engine ~context ~path
               | `dune_project ->
                 Dune_project_lint.lint_file ~dunolint_engine ~context ~path)
         with
         | Continue -> loop files
         | Skip_subtree -> Dunolint_engine.Visitor_decision.Skip_subtree)
    in
    loop files
;;
