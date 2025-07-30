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

module Ordered_set = struct
  type t = Dune.Compilation_mode.t Dunolinter.Ordered_set.t [@@deriving sexp_of]
end

type t = { mutable modes : Ordered_set.t } [@@deriving sexp_of]

let field_name = "modes"

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable_ordered_set
    (struct
      let field_name = field_name
    end)
    (Dune.Compilation_mode)

let create ~modes = { modes }
let modes t = t.modes
let set_modes t ~modes = t.modes <- modes

let read ~sexps_rewriter ~field =
  let modes = Handler.read ~sexps_rewriter ~field in
  { modes = Dunolinter.Ordered_set.canonical_sort (module Dune.Compilation_mode) modes }
;;

let write t = Handler.write t.modes
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.modes ~sexps_rewriter ~field

type predicate = Dune.Library.Modes.Predicate.t

let has_mode t ~mode =
  match
    Dunolinter.Ordered_set.mem
      (module Dune.Compilation_mode)
      t.modes
      mode
      ~evaluator:Dunolinter.Ordered_set.Evaluator.static
  with
  | Known true -> true
  | Known false | Unknown -> false
;;

let eval t ~predicate =
  match (predicate : predicate) with
  | `has_modes modes ->
    Dunolint.Trilang.const (List.for_all modes ~f:(fun mode -> has_mode t ~mode))
  | `has_mode mode -> Dunolint.Trilang.const (has_mode t ~mode)
;;

let insert_mode t ~mode =
  match
    Dunolinter.Ordered_set.mem
      (module Dune.Compilation_mode)
      t.modes
      mode
      ~evaluator:Dunolinter.Ordered_set.Evaluator.static
  with
  | Known true -> ()
  | Known false | Unknown ->
    t.modes <- Dunolinter.Ordered_set.insert (module Dune.Compilation_mode) t.modes mode
;;

let remove_mode t ~mode =
  match
    Dunolinter.Ordered_set.mem
      (module Dune.Compilation_mode)
      t.modes
      mode
      ~evaluator:Dunolinter.Ordered_set.Evaluator.static
  with
  | Known false -> ()
  | Known true | Unknown ->
    t.modes <- Dunolinter.Ordered_set.remove (module Dune.Compilation_mode) t.modes mode
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Modes.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | T (`has_mode mode) ->
        insert_mode t ~mode;
        Ok
      | T (`has_modes modes) ->
        List.iter modes ~f:(fun mode -> insert_mode t ~mode);
        Ok
      | Not (`has_mode mode) ->
        remove_mode t ~mode;
        Ok
      | Not (`has_modes modes) ->
        List.iter modes ~f:(fun mode -> remove_mode t ~mode);
        Ok)
;;

let initialize ~condition =
  let modes =
    let set =
      Dunolinter.Linter.at_positive_enforcing_position condition
      |> List.concat_map ~f:(function
        | `has_modes modes -> modes
        | `has_mode mode -> [ mode ])
      |> Set.of_list (module Dune.Compilation_mode)
    in
    if Set.is_empty set then Set.singleton (module Dune.Compilation_mode) `best else set
  in
  { modes = Dunolinter.Ordered_set.of_set modes }
;;
