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

let field_name = "name"

type t = { mutable name : Dune_project.Name.t } [@@deriving sexp_of]

let create ~name = { name }
let name t = t.name
let set_name t ~name = t.name <- name

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable
    (struct
      let field_name = field_name
    end)
    (Dune_project.Name)

let read ~sexps_rewriter ~field =
  let name = Handler.read ~sexps_rewriter ~field in
  { name }
;;

let write t = Handler.write t.name
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.name ~sexps_rewriter ~field

type predicate = Dune_project.Name.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `equals name -> Dune_project.Name.equal name t.name
   | `is_prefix prefix -> String.is_prefix (Dune_project.Name.to_string t.name) ~prefix
   | `is_suffix suffix -> String.is_suffix (Dune_project.Name.to_string t.name) ~suffix)
  |> Dunolint.Trilang.const
;;

let rec enforce t ~condition =
  match (condition : predicate Blang.t) with
  | Base (`equals name) -> t.name <- name
  | Base (`is_prefix prefix) ->
    let value = Dune_project.Name.to_string t.name in
    if not (String.is_prefix value ~prefix)
    then t.name <- Dune_project.Name.v (prefix ^ value)
  | Not (Base (`is_prefix prefix)) ->
    let value = Dune_project.Name.to_string t.name in
    (match String.chop_prefix value ~prefix with
     | None -> ()
     | Some value -> t.name <- Dune_project.Name.v value)
  | Base (`is_suffix suffix) ->
    let value = Dune_project.Name.to_string t.name in
    if not (String.is_suffix value ~suffix)
    then t.name <- Dune_project.Name.v (value ^ suffix)
  | Not (Base (`is_suffix suffix)) ->
    let value = Dune_project.Name.to_string t.name in
    (match String.chop_suffix value ~suffix with
     | None -> ()
     | Some value -> t.name <- Dune_project.Name.v value)
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Dune_project.Name.Predicate)
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
    match (predicate : Dune_project.Predicate.t) with
    | `name condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `generate_opam_files _ | `implicit_transitive_deps _ -> Dunolint.Trilang.Undefined
  ;;

  let rec enforce (t : t) ~condition =
    match (condition : Dune_project.Predicate.t Blang.t) with
    | (True | False | And _ | If _ | Not _ | Or _) as condition ->
      Dunolinter.Linter.enforce_blang
        (module Dune_project.Predicate)
        t
        ~condition
        ~eval
        ~enforce
    | Base dune_project ->
      (match dune_project with
       | `name condition -> Top.enforce t ~condition
       | `generate_opam_files _ | `implicit_transitive_deps _ -> ())
  ;;
end
