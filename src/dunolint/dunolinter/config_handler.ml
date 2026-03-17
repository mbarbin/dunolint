(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
       raise (Err.E (Sexp_handler.render_sexp_error_exn ~loc exn)))
;;
