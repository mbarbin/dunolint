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

let field_name = "implicit_transitive_deps"

type t = { mutable value : bool } [@@deriving sexp_of]

let create ~implicit_transitive_deps:value = { value }
let value t = t.value
let set_value t ~value = t.value <- value

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable
    (struct
      let field_name = field_name
    end)
    (Bool)

let read ~sexps_rewriter ~field =
  let value = Handler.read ~sexps_rewriter ~field in
  { value }
;;

let write t = Handler.write t.value
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.value ~sexps_rewriter ~field

type predicate = Dune_project.Implicit_transitive_deps.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `equals expected -> Bool.equal expected t.value |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune_project.Implicit_transitive_deps.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals value) ->
        t.value <- not value;
        Ok
      | T (`equals value) ->
        t.value <- value;
        Ok)
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
    | `implicit_transitive_deps condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `generate_opam_files _ | `name _ -> Dunolint.Trilang.Undefined
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune_project.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        match predicate with
        | Not _ -> Eval
        | T dune_project ->
          (match dune_project with
           | `implicit_transitive_deps condition ->
             Top.enforce t ~condition;
             Ok
           | `generate_opam_files _ | `name _ -> Unapplicable))
  ;;
end
