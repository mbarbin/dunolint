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

let field_name = "generate_opam_files"

type t = { args : Sexp.t list } [@@deriving sexp_of]

let create () = { args = [] }

module Handler = Dunolinter.Sexp_handler.Make_sexp_list (struct
    let field_name = field_name
  end)

let read ~sexps_rewriter ~field =
  let args = Handler.read ~sexps_rewriter ~field in
  { args }
;;

let write t = Handler.write t.args
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.args ~sexps_rewriter ~field

type predicate = Dune_project.Generate_opam_files.Predicate.t

let eval _t ~predicate =
  match (predicate : predicate) with
  | `is_present -> true |> Dunolint.Trilang.const
;;

let rec enforce t ~condition =
  match (condition : predicate Blang.t) with
  | Base `is_present -> ()
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Dune_project.Generate_opam_files.Predicate)
      t
      ~condition
      ~eval
      ~enforce
;;

module Top = struct
  type nonrec t = t

  let eval = eval
  let enforce = enforce
end

module Linter = struct
  type t = Top.t
  type predicate = Dune_project.Predicate.t

  let eval (t : t) ~predicate =
    match (predicate : predicate) with
    | `generate_opam_files condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `implicit_transitive_deps _ | `name _ -> Dunolint.Trilang.Undefined
  ;;

  let rec enforce (t : t) ~condition =
    match (condition : predicate Blang.t) with
    | (True | False | And _ | If _ | Not _ | Or _) as condition ->
      Dunolinter.Linter.enforce_blang
        (module Dune_project.Predicate)
        t
        ~condition
        ~eval
        ~enforce
    | Base dune_project ->
      (match dune_project with
       | `generate_opam_files condition -> Top.enforce t ~condition
       | `implicit_transitive_deps _ | `name _ -> ())
  ;;
end
