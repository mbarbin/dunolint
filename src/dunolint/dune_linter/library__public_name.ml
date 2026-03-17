(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Public_name.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals public_name) ->
        t.public_name <- public_name;
        Ok
      | T (`is_prefix prefix) ->
        let value = Dune.Library.Public_name.to_string t.public_name in
        if not (String.is_prefix value ~prefix)
        then
          t.public_name
          <- Dune.Library.Public_name.v
               (Dunolinter.Linter.public_name_is_prefix value ~prefix);
        Ok
      | Not (`is_prefix prefix) ->
        let value = Dune.Library.Public_name.to_string t.public_name in
        (match String.chop_prefix value ~prefix with
         | None -> Ok
         | Some value ->
           t.public_name <- Dune.Library.Public_name.v value;
           Ok)
      | T (`is_suffix suffix) ->
        let value = Dune.Library.Public_name.to_string t.public_name in
        if not (String.is_suffix value ~suffix)
        then t.public_name <- Dune.Library.Public_name.v (value ^ suffix);
        Ok
      | Not (`is_suffix suffix) ->
        let value = Dune.Library.Public_name.to_string t.public_name in
        (match String.chop_suffix value ~suffix with
         | None -> Ok
         | Some value ->
           t.public_name <- Dune.Library.Public_name.v value;
           Ok))
;;
