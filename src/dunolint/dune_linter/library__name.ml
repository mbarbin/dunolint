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

type t = { mutable name : Dune.Library.Name.t } [@@deriving sexp_of]

let field_name = "name"
let name t = t.name

module Handler = Dunolinter.Sexp_handler.Make_atom (struct
    let field_name = field_name
  end)

let create ~name = { name }

let read ~sexps_rewriter ~field =
  let name = Handler.read ~sexps_rewriter ~field in
  create ~name:(Dune.Library.Name.v name)
;;

let write t = Handler.write (Dune.Library.Name.to_string t.name)

let rewrite t ~sexps_rewriter ~field =
  Handler.rewrite (Dune.Library.Name.to_string t.name) ~sexps_rewriter ~field
;;

type predicate = Dune.Library.Name.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `equals name -> Dune.Library.Name.equal name t.name
   | `is_prefix prefix -> String.is_prefix (Dune.Library.Name.to_string t.name) ~prefix
   | `is_suffix suffix -> String.is_suffix (Dune.Library.Name.to_string t.name) ~suffix)
  |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Name.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals name) ->
        t.name <- name;
        Ok
      | T (`is_prefix prefix) ->
        let value = Dune.Library.Name.to_string t.name in
        if not (String.is_prefix value ~prefix)
        then t.name <- Dune.Library.Name.v (prefix ^ value);
        Ok
      | Not (`is_prefix prefix) ->
        let value = Dune.Library.Name.to_string t.name in
        (match String.chop_prefix value ~prefix with
         | None -> Ok
         | Some value ->
           t.name <- Dune.Library.Name.v value;
           Ok)
      | T (`is_suffix suffix) ->
        let value = Dune.Library.Name.to_string t.name in
        if not (String.is_suffix value ~suffix)
        then t.name <- Dune.Library.Name.v (value ^ suffix);
        Ok
      | Not (`is_suffix suffix) ->
        let value = Dune.Library.Name.to_string t.name in
        (match String.chop_suffix value ~suffix with
         | None -> Ok
         | Some value ->
           t.name <- Dune.Library.Name.v value;
           Ok))
;;
