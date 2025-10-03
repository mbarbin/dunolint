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

let load_config_exn ~filename =
  let contents = In_channel.read_all filename in
  match Parsexp.Many_and_positions.parse_string contents with
  | Error parse_error ->
    let position = Parsexp.Parse_error.position parse_error in
    let message = Parsexp.Parse_error.message parse_error in
    let loc =
      Sexp_handler.loc_of_parsexp_range
        ~filename
        { start_pos = position; end_pos = position }
    in
    Err.raise ~loc [ Pp.text message ]
  | Ok (sexps, positions) ->
    (match Dunolint.Config.of_stanzas sexps with
     | t -> t
     | exception Sexp.Of_sexp_error (exn, sub) ->
       let range =
         match Parsexp.Positions.find_sub_sexp_in_list_phys positions sexps ~sub with
         | Some _ as range -> range
         | None -> None [@coverage off]
       in
       let loc =
         match range with
         | Some range -> Sexp_handler.loc_of_parsexp_range ~filename range
         | None -> Loc.of_file ~path:(Fpath.v filename) [@coverage off]
       in
       let message =
         match exn with
         | Failure str ->
           Pp.text (if String.is_suffix str ~suffix:"." then str else str ^ ".")
         | exn -> Err.exn exn [@coverage off]
       in
       Err.raise ~loc [ message ])
;;
