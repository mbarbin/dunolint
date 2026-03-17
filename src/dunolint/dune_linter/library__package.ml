(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = { mutable name : Dune.Package.Name.t } [@@deriving sexp_of]

let field_name = "package"

module Handler = Dunolinter.Sexp_handler.Make_atom (struct
    let field_name = field_name
  end)

let create ~name = { name }

let read ~sexps_rewriter ~field =
  let name = Handler.read ~sexps_rewriter ~field in
  create ~name:(Dune.Package.Name.v name)
;;

let write t = Handler.write (Dune.Package.Name.to_string t.name)

let rewrite t ~sexps_rewriter ~field =
  Handler.rewrite (Dune.Package.Name.to_string t.name) ~sexps_rewriter ~field
;;

type predicate = Dune.Library.Package.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `equals name -> Dune.Package.Name.equal name t.name
   | `is_prefix prefix -> String.is_prefix (Dune.Package.Name.to_string t.name) ~prefix
   | `is_suffix suffix -> String.is_suffix (Dune.Package.Name.to_string t.name) ~suffix)
  |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Package.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals name) ->
        t.name <- name;
        Ok
      | T (`is_prefix prefix) ->
        let value = Dune.Package.Name.to_string t.name in
        if not (String.is_prefix value ~prefix)
        then t.name <- Dune.Package.Name.v (prefix ^ value);
        Ok
      | Not (`is_prefix prefix) ->
        let value = Dune.Package.Name.to_string t.name in
        (match String.chop_prefix value ~prefix with
         | None -> Ok
         | Some value ->
           t.name <- Dune.Package.Name.v value;
           Ok)
      | T (`is_suffix suffix) ->
        let value = Dune.Package.Name.to_string t.name in
        if not (String.is_suffix value ~suffix)
        then t.name <- Dune.Package.Name.v (value ^ suffix);
        Ok
      | Not (`is_suffix suffix) ->
        let value = Dune.Package.Name.to_string t.name in
        (match String.chop_suffix value ~suffix with
         | None -> Ok
         | Some value ->
           t.name <- Dune.Package.Name.v value;
           Ok))
;;
