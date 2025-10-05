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
    | Present of Dunolint.Config.t
    | Error of Err.t
end

type t = Load_result.t Hashtbl.M(Relative_path).t

let create () : t = Hashtbl.create (module Relative_path)

let load_config_in_dir (t : t) ~dir : Load_result.t =
  let config_path = Relative_path.extend dir (Fsegment.v "dunolint") in
  match Hashtbl.find t config_path with
  | Some result ->
    (* CR-soon mbarbin: At the moment we haven't exercised in tests execution
       paths where the cache is useful. This is left as follow-up work. *)
    result
    [@coverage off]
  | None ->
    let result : Load_result.t =
      match
        match (Unix.stat (Relative_path.to_string config_path)).st_kind with
        | exception Unix.Unix_error (ENOENT, _, _) -> `Absent
        | file_kind ->
          (match[@coverage off] file_kind with
           | S_REG | S_LNK -> `Present
           | S_DIR | S_CHR | S_BLK | S_FIFO | S_SOCK -> `Not_a_file)
      with
      | `Absent | `Not_a_file ->
        Log.debug ~src (fun () ->
          Pp.O.
            [ Pp.text "Config file does not exist at "
              ++ Pp_tty.path (module Relative_path) config_path
              ++ Pp.text "."
            ]);
        Absent
      | `Present ->
        (try
           let config =
             Dunolinter.Config_handler.load_config_exn
               ~filename:(Relative_path.to_string config_path)
           in
           Log.info ~src (fun () ->
             Pp.O.
               [ Pp.text "Loaded dunolint config from "
                 ++ Pp_tty.path (module Relative_path) config_path
                 ++ Pp.text "."
               ]);
           Present config
         with
         | Err.E err -> Error err
         | exn ->
           (let err =
              Err.create
                Pp.O.
                  [ Pp.textf "Failed to load config at "
                    ++ Pp_tty.path (module Relative_path) config_path
                    ++ Pp.text "."
                  ; Err.exn exn
                  ]
            in
            Error err)
           [@coverage off])
    in
    Hashtbl.set t ~key:config_path ~data:result;
    result
;;
