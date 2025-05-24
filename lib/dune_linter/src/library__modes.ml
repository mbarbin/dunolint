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

type t = { mutable modes : Dune.Library.Modes.t } [@@deriving sexp_of]

let field_name = "modes"

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable_list
    (struct
      let field_name = field_name
    end)
    (Dune.Compilation_mode)

let create ~modes = { modes }
let modes t = t.modes
let set_modes t ~modes = t.modes <- modes

let read ~sexps_rewriter ~field =
  let modes = Handler.read ~sexps_rewriter ~field in
  create ~modes:(Dune.Library.Modes.of_list modes)
;;

let write t = Handler.write (Set.to_list t.modes)

let rewrite t ~sexps_rewriter ~field =
  Handler.rewrite (Set.to_list t.modes) ~sexps_rewriter ~field
;;

type predicate = Dune.Library.Modes.Predicate.t

let eval t ~predicate =
  (match (predicate : predicate) with
   | `equals modes -> Set.equal t.modes modes
   | `has_mode mode -> Set.mem t.modes mode)
  |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Library.Modes.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals modes) ->
        t.modes <- modes;
        Ok
      | T (`has_mode mode) ->
        t.modes <- Set.add t.modes mode;
        Ok
      | Not (`has_mode mode) ->
        t.modes <- Set.remove t.modes mode;
        Ok)
;;

let initialize ~condition =
  let modes =
    let set =
      List.map (Dunolinter.Linter.at_positive_enforcing_position condition) ~f:(function
        | `equals modes -> modes
        | `has_mode mode -> Set.singleton (module Dune.Compilation_mode) mode)
      |> Set.union_list (module Dune.Compilation_mode)
    in
    if Set.is_empty set then Set.singleton (module Dune.Compilation_mode) `best else set
  in
  { modes }
;;
