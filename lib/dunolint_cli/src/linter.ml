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

let lint_stanza ~rules ~stanza ~(return : _ With_return.return) =
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
        | `skip_subtree -> return.return `skip_subtree))
;;

module Lint_file (Linter : Dunolinter.S) = struct
  let lint_file ~dunolint_engine ~rules ~(path : Relative_path.t) =
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
              Linter.visit linter ~f:(fun stanza -> lint_stanza ~rules ~stanza ~return);
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

let visit_directory ~dunolint_engine ~config ~parent_dir ~files =
  match
    match Dunolint.Config.skip_subtree config with
    | None -> `return
    | Some condition ->
      Dunolint.Rule.eval condition ~f:(fun (`path condition) ->
        Dunolinter.eval_path ~path:parent_dir ~condition)
  with
  | `enforce nothing -> Nothing.unreachable_code nothing [@coverage off]
  | `skip_subtree -> Dunolint_engine.Visitor_decision.Skip_subtree
  | `return ->
    let rules = Dunolint.Config.rules config in
    let rec loop = function
      | [] -> Dunolint_engine.Visitor_decision.Continue
      | file :: files ->
        let path = Relative_path.extend parent_dir (Fsegment.v file) in
        (match
           match Dunolint.Linted_file_kind.of_string file with
           | Error (`Msg _) -> Visitor_decision.Continue
           | Ok linted_file_kind ->
             (match linted_file_kind with
              | `dune -> Dune_lint.lint_file ~dunolint_engine ~rules ~path
              | `dune_project -> Dune_project_lint.lint_file ~dunolint_engine ~rules ~path)
         with
         | Continue -> loop files
         | Skip_subtree -> Dunolint_engine.Visitor_decision.Skip_subtree)
    in
    loop files
;;
