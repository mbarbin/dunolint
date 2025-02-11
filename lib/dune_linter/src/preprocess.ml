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

module State = struct
  type t =
    | No_preprocessing
    | Pps of Pps.t
    | Unhandled of Sexp.t
  [@@deriving sexp_of]
end

type t = { mutable state : State.t } [@@deriving sexp_of]

let create ?pps () =
  { state =
      (match pps with
       | Some pps -> Pps pps
       | None -> No_preprocessing)
  }
;;

let field_name = "preprocess"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ Atom "no_preprocessing" ] -> { state = No_preprocessing }
  | [ (List (Atom "pps" :: _) as field) ] ->
    let pps = Pps.read ~sexps_rewriter ~field in
    { state = Pps pps }
  | [ sexp ] -> { state = Unhandled sexp }
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) field_name
          ++ Pp.text " field value."
        ]
;;

let write (t : t) =
  let args =
    match t.state with
    | No_preprocessing -> [ Sexp.Atom "no_preprocessing" ]
    | Pps pps -> [ Pps.write pps ]
    | Unhandled sexp -> [ sexp ]
  in
  Sexp.List (Atom field_name :: args)
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  match args with
  | [ (Atom "no_preprocessing" as arg) ] ->
    (match t.state with
     | No_preprocessing -> ()
     | Pps pps ->
       File_rewriter.replace
         file_rewriter
         ~range:(Sexps_rewriter.range sexps_rewriter arg)
         ~text:(Pps.write pps |> Sexp.to_string)
     | Unhandled _ -> ())
  | [ (List (Atom "pps" :: _) as field) ] ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    (match t.state with
     | No_preprocessing ->
       File_rewriter.replace file_rewriter ~range:(Loc.range loc) ~text:"no_preprocessing"
     | Pps pps -> Pps.rewrite pps ~sexps_rewriter ~field
     | Unhandled _ -> ())
  | [ arg ] ->
    let loc = Sexps_rewriter.loc sexps_rewriter arg in
    (match t.state with
     | No_preprocessing ->
       File_rewriter.replace file_rewriter ~range:(Loc.range loc) ~text:"no_preprocessing"
     | Pps pps ->
       File_rewriter.replace
         file_rewriter
         ~range:(Sexps_rewriter.range sexps_rewriter arg)
         ~text:(Pps.write pps |> Sexp.to_string)
     | Unhandled _ -> ())
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) field_name
          ++ Pp.text " field value."
        ]
;;

type predicate = Dune.Preprocess.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `no_preprocessing ->
    (match t.state with
     | No_preprocessing -> true |> Dunolint.Trilang.const
     | Pps _ | Unhandled _ -> false |> Dunolint.Trilang.const)
  | `pps condition ->
    (match t.state with
     | Pps pps ->
       Dunolint.Trilang.eval condition ~f:(fun predicate -> Pps.eval pps ~predicate)
     | No_preprocessing | Unhandled _ -> false |> Dunolint.Trilang.const)
;;

let rec enforce t ~condition =
  match (condition : predicate Blang.t) with
  | Base `no_preprocessing -> t.state <- No_preprocessing
  | Base (`pps condition) ->
    (match t.state with
     | Pps pps -> Pps.enforce pps ~condition
     | No_preprocessing | Unhandled _ ->
       let pps = Pps.create ~args:[] in
       Pps.enforce pps ~condition;
       t.state <- Pps pps)
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Dune.Preprocess.Predicate)
      t
      ~condition
      ~eval
      ~enforce
;;
