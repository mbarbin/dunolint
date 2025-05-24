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

type t = { mutable backend : Dune.Instrumentation.Backend.Name.t } [@@deriving sexp_of]

let create ~backend = { backend }
let backend t = t.backend
let set_backend t ~backend = t.backend <- backend
let field_name = "instrumentation"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match
    List.find_map args ~f:(function
      | List [ Atom "backend"; Atom name ] ->
        Some (Dune.Instrumentation.Backend.Name.v name)
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
  Sexp.List
    [ Atom field_name
    ; List
        [ Atom "backend"; Atom (Dune.Instrumentation.Backend.Name.to_string t.backend) ]
    ]
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  List.iter args ~f:(function
    | List [ Atom "backend"; (Atom _ as name) ] ->
      let range = Sexps_rewriter.range sexps_rewriter name in
      File_rewriter.replace
        file_rewriter
        ~range
        ~text:(Dune.Instrumentation.Backend.Name.to_string t.backend)
    | _ -> ())
;;

type predicate = Dune.Instrumentation.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `backend name ->
    Dune.Instrumentation.Backend.Name.equal name t.backend |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Instrumentation.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`backend _) -> Eval
      | T (`backend name) ->
        t.backend <- name;
        Ok)
;;

let default_backend = Dune.Instrumentation.Backend.Name.v "bisect_ppx"

let initialize ~condition =
  let backend =
    List.find_map
      (Dunolinter.Linter.at_positive_enforcing_position condition)
      ~f:(function `backend name -> Some name)
    |> Option.value ~default:default_backend
  in
  { backend }
;;
