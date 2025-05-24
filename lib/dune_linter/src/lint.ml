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

type t = { pps : Pps.t } [@@deriving sexp_of]

let create ?pps () =
  { pps =
      (match pps with
       | Some pps -> pps
       | None -> Pps.create ~args:[])
  }
;;

let field_name = "lint"

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ (List (Atom "pps" :: _) as field) ] ->
    let pps = Pps.read ~sexps_rewriter ~field in
    { pps }
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) "lint"
          ++ Pp.text " field value."
        ]
;;

let write (t : t) = Sexp.List [ Atom field_name; Pps.write t.pps ]

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  match args with
  | [ (List (Atom "pps" :: _) as field) ] -> Pps.rewrite t.pps ~sexps_rewriter ~field
  | _ ->
    let loc = Sexps_rewriter.loc sexps_rewriter field in
    Err.raise
      ~loc
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) "lint"
          ++ Pp.text " field value."
        ]
;;

type predicate = Dune.Lint.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `pps condition ->
    Dunolint.Trilang.eval condition ~f:(fun predicate -> Pps.eval t.pps ~predicate)
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Lint.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`pps _) -> Eval
      | T (`pps condition) ->
        Pps.enforce t.pps ~condition;
        Ok)
;;
