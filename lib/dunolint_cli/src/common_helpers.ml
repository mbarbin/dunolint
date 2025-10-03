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
  Arg.named_opt [ "below" ] (Param.validated_string (module Fpath)) ~docv:"PATH" ~doc
;;

let skip_subtrees ~globs =
  List.concat
    [ List.map globs ~f:Dunolint.Glob.v
    ; List.concat_map
        ~f:(fun pat -> [ Dunolint.Glob.v ("**/" ^ pat); Dunolint.Glob.v pat ])
        [ ".git/"
        ; "_build/"
        ; "_opam/"
        ; "_coverage/"
        ; "node_modules/"
        ; "doc/build/"
        ; ".docusaurus/"
        ; "*.t/"
        ]
    ]
;;

let load_config_exn ~filename =
  let contents = In_channel.read_all filename in
  match Parsexp.Many_and_positions.parse_string contents with
  | Error parse_error ->
    let position = Parsexp.Parse_error.position parse_error in
    let message = Parsexp.Parse_error.message parse_error in
    let loc =
      Dunolinter.Sexp_handler.loc_of_parsexp_range
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
         | Some range -> Dunolinter.Sexp_handler.loc_of_parsexp_range ~filename range
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

let load_config_opt_exn ~config ~append_extra_rules =
  let config =
    match config with
    | Some filename -> load_config_exn ~filename
    | None ->
      let cwd = Unix.getcwd () |> Absolute_path.v in
      let default_file = Absolute_path.extend cwd (Fsegment.v "dunolint") in
      let filename = Absolute_path.to_string default_file in
      if Stdlib.Sys.file_exists filename
      then load_config_exn ~filename:"dunolint"
      else
        Dunolint.Config.V1.create [ `skip_paths (skip_subtrees ~globs:[]) ]
        |> Dunolint.Config.v1
  in
  let config =
    match Dunolint.Config.Private.view config with
    | `v0 config ->
      Dunolint.Config.V0.create
        ?skip_subtree:(Dunolint.Config.V0.skip_subtree config)
        ~rules:(Dunolint.Config.V0.rules config @ append_extra_rules)
        ()
      |> Dunolint.Config.v0
    | `v1 config ->
      Dunolint.Config.V1.create
        (List.concat
           [ List.map (Dunolint.Config.V1.skip_paths config) ~f:(fun globs ->
               `skip_paths globs)
           ; List.map
               (Dunolint.Config.V1.rules config @ append_extra_rules)
               ~f:(fun rule -> `rule rule)
           ])
      |> Dunolint.Config.v1
  in
  config
;;

let ancestors_directories ~(path : Relative_path.t) =
  let segs = Fpath.segs (path :> Fpath.t) in
  List.init (List.length segs) ~f:(fun i ->
    List.take segs i
    |> List.map ~f:Fsegment.v
    |> Relative_path.of_list
    |> Relative_path.to_dir_path)
  |> List.filter ~f:(fun path -> not (Relative_path.equal Relative_path.empty path))
;;

let root =
  let open Command.Std in
  let+ root =
    Arg.named_opt
      [ "root" ]
      (Param.validated_string (module Fpath))
      ~docv:"DIR"
      ~doc:"Use this directory as dune workspace root instead of guessing it."
  in
  Option.map root ~f:(fun root ->
    match Fpath.classify root with
    | `Absolute path -> path
    | `Relative path ->
      let cwd = Unix.getcwd () |> Absolute_path.v in
      Absolute_path.append cwd path)
;;

let relativize ~workspace_root ~cwd ~path =
  let path = Absolute_path.relativize ~root:cwd path in
  match
    Absolute_path.chop_prefix path ~prefix:(workspace_root |> Workspace_root.path)
  with
  | Some relative_path -> relative_path
  | None ->
    Err.raise
      Pp.O.
        [ Pp.text "Path "
          ++ Pp_tty.path (module Absolute_path) path
          ++ Pp.text " is not in dune workspace."
        ]
;;
