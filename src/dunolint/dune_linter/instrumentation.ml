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

type t = { mutable backend : Dune.Instrumentation.Backend.t } [@@deriving sexp_of]

let create ~backend = { backend }
let backend t = t.backend
let set_backend t ~backend = t.backend <- backend
let field_name = "instrumentation"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match
    List.find_map args ~f:(function
      | List (Atom "backend" :: Atom name :: flag_sexps) ->
        Some
          (Dune.Instrumentation.Backend.create
             ~name:(Dune.Instrumentation.Backend.Name.v name)
             ~flags:(List.map flag_sexps ~f:Dune.Instrumentation.Backend.Flag.t_of_sexp))
      | _ -> None)
  with
  | Some backend -> create ~backend
  | None ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Required "
          ++ Pp_tty.kwd (module String) "backend"
          ++ Pp.text " value in instrumentation."
        ]
;;

let write (t : t) =
  let name = Dune.Instrumentation.Backend.name t.backend in
  let flags = Dune.Instrumentation.Backend.flags t.backend in
  Sexp.List
    [ Atom field_name
    ; List
        (Atom "backend"
         :: Atom (Dune.Instrumentation.Backend.Name.to_string name)
         :: List.map flags ~f:Dune.Instrumentation.Backend.Flag.sexp_of_t)
    ]
;;

let is_atom_and_equal sexp ~to_:text =
  match (sexp : Sexp.t) with
  | Atom s -> String.equal s text
  | List _ -> false
;;

let rewrite_flags ~file_rewriter ~sexps_rewriter ~prev_sexp ~desired ~actual =
  let rec iter ~prev_sexp desired actual =
    match desired, actual with
    | [], [] -> ()
    | desired_flag :: rest_desired, actual_flag :: rest_actual ->
      if not (is_atom_and_equal actual_flag ~to_:desired_flag)
      then
        File_rewriter.replace
          file_rewriter
          ~range:(Sexps_rewriter.range sexps_rewriter actual_flag)
          ~text:desired_flag;
      iter ~prev_sexp:actual_flag rest_desired rest_actual
    | rest_desired, [] ->
      let insert_offset = Sexps_rewriter.stop_offset sexps_rewriter prev_sexp in
      let text =
        List.map rest_desired ~f:(fun flag -> " " ^ flag) |> String.concat ~sep:""
      in
      File_rewriter.insert file_rewriter ~offset:insert_offset ~text
    | [], (_ :: _ as remaining_actual) ->
      let last_arg = List.last_exn remaining_actual in
      let range =
        { Loc.Range.start = Sexps_rewriter.stop_offset sexps_rewriter prev_sexp
        ; stop = Sexps_rewriter.stop_offset sexps_rewriter last_arg
        }
      in
      File_rewriter.remove file_rewriter ~range
  in
  iter ~prev_sexp desired actual
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let desired_name =
    Dune.Instrumentation.Backend.Name.to_string
      (Dune.Instrumentation.Backend.name t.backend)
  in
  let desired_flags = Dune.Instrumentation.Backend.flags t.backend in
  List.iter args ~f:(function
    | List (Atom "backend" :: (Atom actual_name as name_sexp) :: actual_flags) ->
      if not (String.equal actual_name desired_name)
      then
        File_rewriter.replace
          file_rewriter
          ~range:(Sexps_rewriter.range sexps_rewriter name_sexp)
          ~text:desired_name;
      rewrite_flags
        ~file_rewriter
        ~sexps_rewriter
        ~prev_sexp:name_sexp
        ~desired:desired_flags
        ~actual:actual_flags
    | _ -> ())
;;

type predicate = Dune.Instrumentation.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `backend backend ->
    Dune.Instrumentation.Backend.equal backend t.backend |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Instrumentation.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`backend _) -> Eval
      | T (`backend backend) ->
        t.backend <- backend;
        Ok)
;;

let default_backend = Dune.Instrumentation.Backend.v "bisect_ppx"

let initialize ~condition =
  let backend =
    Dunolinter.Linter.find_init_value condition ~f:(function `backend backend ->
        Some backend)
    |> Option.value ~default:default_backend
  in
  { backend }
;;
