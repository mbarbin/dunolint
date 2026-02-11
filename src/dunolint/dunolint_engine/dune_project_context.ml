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
