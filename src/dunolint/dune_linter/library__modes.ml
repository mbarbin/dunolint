(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
  | `mem modes | `has_modes modes ->
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
      | T (`mem modes) | T (`has_modes modes) ->
        List.iter modes ~f:(fun mode -> insert_mode t ~mode);
        Ok
      | T (`has_mode mode) ->
        insert_mode t ~mode;
        Ok
      | Not (`mem modes) | Not (`has_modes modes) ->
        List.iter modes ~f:(fun mode -> remove_mode t ~mode);
        Ok
      | Not (`has_mode mode) ->
        remove_mode t ~mode;
        Ok)
;;
