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

type t = { mutable public_name : Dune.Library.Public_name.t } [@@deriving sexp_of]

let field_name = "public_name"

module Handler = Dunolinter.Sexp_handler.Make_atom (struct
    let field_name = field_name
  end)

let create ~public_name = { public_name }

let read ~sexps_rewriter ~field =
  let public_name = Handler.read ~sexps_rewriter ~field in
  create ~public_name:(Dune.Library.Public_name.v public_name)
;;

let write t = Handler.write (Dune.Library.Public_name.to_string t.public_name)

let rewrite t ~sexps_rewriter ~field =
  Handler.rewrite
    (Dune.Library.Public_name.to_string t.public_name)
    ~sexps_rewriter
    ~field
;;

type predicate = Dune.Library.Public_name.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `equals public_name -> Dune.Library.Public_name.equal public_name t.public_name
   | `is_prefix prefix ->
     String.is_prefix (Dune.Library.Public_name.to_string t.public_name) ~prefix
   | `is_suffix suffix ->
     String.is_suffix (Dune.Library.Public_name.to_string t.public_name) ~suffix)
  |> Dunolint.Trilang.const
;;

let rec enforce t ~condition =
  match (condition : Dune.Library.Public_name.Predicate.t Blang.t) with
  | Base (`equals public_name) -> t.public_name <- public_name
  | Base (`is_prefix prefix) ->
    let value = Dune.Library.Public_name.to_string t.public_name in
    if not (String.is_prefix value ~prefix)
    then
      t.public_name
      <- Dune.Library.Public_name.v
           (Dunolinter.Linter.public_name_is_prefix value ~prefix)
  | Not (Base (`is_prefix prefix)) ->
    let value = Dune.Library.Public_name.to_string t.public_name in
    (match String.chop_prefix value ~prefix with
     | None -> ()
     | Some value -> t.public_name <- Dune.Library.Public_name.v value)
  | Base (`is_suffix suffix) ->
    let value = Dune.Library.Public_name.to_string t.public_name in
    if not (String.is_suffix value ~suffix)
    then t.public_name <- Dune.Library.Public_name.v (value ^ suffix)
  | Not (Base (`is_suffix suffix)) ->
    let value = Dune.Library.Public_name.to_string t.public_name in
    (match String.chop_suffix value ~suffix with
     | None -> ()
     | Some value -> t.public_name <- Dune.Library.Public_name.v value)
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Dune.Library.Public_name.Predicate)
      t
      ~condition
      ~eval
      ~enforce
;;
