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

let src = Logs.Src.create "dunolint" ~doc:"dunolint"

module Load_result = struct
  type t =
    | Absent
    | Present of
        (Dune_project_context.t, Dune_project_context.Invalid_dune_project.t) Result.t
end

type t = Load_result.t Hashtbl.M(Relative_path).t

let create () : t = Hashtbl.create (module Relative_path)

let load_dune_project_in_dir (t : t) ~dir : Load_result.t =
  let file_path = Relative_path.extend dir (Fsegment.v "dune-project") in
  match Hashtbl.find t file_path with
  | Some load_result -> load_result
  | None ->
    let parsing_result =
      match
        match (Unix.stat (Relative_path.to_string file_path)).st_kind with
        | exception Unix.Unix_error (ENOENT, _, _) -> `Absent
        | file_kind ->
          (match[@coverage off] file_kind with
           | S_REG | S_LNK -> `Present
           | S_DIR | S_CHR | S_BLK | S_FIFO | S_SOCK -> `Not_a_file)
      with
      | `Absent -> `Absent
      | `Not_a_file -> `Absent [@coverage off]
      | `Present ->
        `Present
          (try
             let original_contents =
               In_channel.read_all (file_path |> Relative_path.to_string)
             in
             Dune_project_context.create ~path:file_path ~original_contents
           with
           | exn ->
             (let err =
                Err.create
                  ~loc:(Loc.of_file ~path:(file_path :> Fpath.t))
                  [ Pp.text "Failed to load dune-project file."; Err.exn exn ]
              in
              Error err)
             [@coverage off])
    in
    let load_result : Load_result.t =
      match parsing_result with
      | `Absent ->
        Log.debug ~src (fun () ->
          Pp.O.
            [ Pp.text "Dune project file does not exist at "
              ++ Pp_tty.path (module Relative_path) file_path
              ++ Pp.text "."
            ]);
        Absent
      | `Present (Ok _ as ok) ->
        Log.info ~src (fun () ->
          Pp.O.
            [ Pp.text "Loaded dune-project file from "
              ++ Pp_tty.path (module Relative_path) file_path
              ++ Pp.text "."
            ]);
        Present ok
      | `Present (Error err) ->
        Err.emit err ~level:Info;
        Present (Error (Dune_project_context.Invalid_dune_project.acknowledge err))
    in
    Hashtbl.set t ~key:file_path ~data:load_result;
    load_result
;;
