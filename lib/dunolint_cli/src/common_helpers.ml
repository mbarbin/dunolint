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

let sexpable_param (type a) (module M : Sexpable.S with type t = a) =
  let module Validate = struct
    type t = a

    let of_string str =
      try Ok (Parsexp.Single.parse_string_exn str |> M.t_of_sexp) with
      | exn -> Error (`Msg (Exn.to_string exn))
    ;;

    let to_string t =
      (* This would be used to print default value, currently not exercised. *)
      Sexp.to_string_mach (M.sexp_of_t t) [@coverage off]
    ;;
  end
  in
  Command.Param.validated_string (module Validate)
;;

let below ~doc =
  let open Command.Std in
  Arg.named_opt
    [ "below" ]
    (Param.validated_string (module Relative_path))
    ~docv:"PATH"
    ~doc
;;

let skip_subtree ~globs =
  let open Dunolint.Config.Std in
  cond
    [ ( path
          (or_
             (List.concat
                [ List.map globs ~f:glob
                ; List.concat_map
                    ~f:(fun pat -> [ glob ("**/" ^ pat); glob pat ])
                    [ ".git/"
                    ; "_build/"
                    ; "_opam/"
                    ; "_coverage/"
                    ; "node_modules/"
                    ; "doc/build/"
                    ; ".docusaurus/"
                    ; "*.t/"
                    ]
                ]))
      , skip_subtree )
    ]
;;

let error_message_cleanup_pattern =
  lazy (Re.Perl.compile_pat {|^(?:[^/]*[/])*([^/]*)\.ml\.([^.]*)\.[^:]*:(.*)$|})
;;

let clean_up_error_message str =
  let pattern = Lazy.force error_message_cleanup_pattern in
  match Re.exec_opt pattern str with
  | None -> str
  | Some match_info ->
    let basename = Re.Group.get match_info 1 in
    let module_name = Re.Group.get match_info 2 in
    let rest = Re.Group.get match_info 3 in
    Printf.sprintf "%s.%s:%s" basename module_name rest
;;

let loc_of_parsexp_range ~filename (range : Parsexp.Positions.range) =
  let source_code_position ({ line; col; offset } : Parsexp.Positions.pos) =
    { Lexing.pos_fname = filename
    ; pos_lnum = line
    ; pos_cnum = offset
    ; pos_bol = offset - col
    }
  in
  Loc.create (source_code_position range.start_pos, source_code_position range.end_pos)
;;

let load_config_exn ~filename =
  let contents = In_channel.read_all filename in
  match Parsexp.Single_and_positions.parse_string contents with
  | Error parse_error ->
    let position = Parsexp.Parse_error.position parse_error in
    let message = Parsexp.Parse_error.message parse_error in
    let loc =
      loc_of_parsexp_range ~filename { start_pos = position; end_pos = position }
    in
    Err.raise ~loc [ Pp.text message ]
  | Ok (sexp, positions) ->
    (match Parsexp.Conv_single.conv (sexp, positions) Dunolint.Config.t_of_sexp with
     | Ok t -> t
     | Error of_sexp_error ->
       let range =
         match Parsexp.Of_sexp_error.location of_sexp_error with
         | Some _ as range -> range
         | None ->
           (let sub = Parsexp.Of_sexp_error.sub_sexp of_sexp_error in
            (match Parsexp.Positions.find_sub_sexp_phys positions sexp ~sub with
             | Some _ as range -> range
             | None -> None))
           [@coverage off]
       in
       let loc =
         match range with
         | Some range -> loc_of_parsexp_range ~filename range
         | None -> Loc.of_file ~path:(Fpath.v filename) [@coverage off]
       in
       let message =
         match Parsexp.Of_sexp_error.user_exn of_sexp_error with
         | Failure str -> clean_up_error_message str
         | exn -> Exn.to_string exn [@coverage off]
       in
       Err.raise ~loc [ Pp.text message ])
;;
