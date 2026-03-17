(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = { dune_lang_version : Dune_project.Dune_lang_version.t option }

let dune_lang_version t = t.dune_lang_version

let create ~(path : Relative_path.t) ~original_contents =
  match Sexps_rewriter.create ~path:(path :> Fpath.t) ~original_contents with
  | Error { loc; message } -> Error (Err.create ~loc [ Pp.text message ])
  | Ok sexps_rewriter ->
    let dune_lang_version = ref None in
    let (_ : bool) =
      List.exists (Sexps_rewriter.original_sexps sexps_rewriter) ~f:(fun stanza ->
        match stanza with
        | List [ Atom "lang"; Atom "dune"; Atom _ ] ->
          let () =
            try
              let linter =
                Dune_project_linter.Dune_lang_version.read ~sexps_rewriter ~field:stanza
              in
              dune_lang_version
              := Some (Dune_project_linter.Dune_lang_version.dune_lang_version linter)
            with
            | Err.E _ -> ()
          in
          true
        | _ -> false)
    in
    Ok { dune_lang_version = !dune_lang_version }
;;

module Invalid_dune_project = struct
  type t = Invalid_dune_project

  let acknowledge err =
    ignore (err : Err.t);
    Invalid_dune_project
  ;;
end
