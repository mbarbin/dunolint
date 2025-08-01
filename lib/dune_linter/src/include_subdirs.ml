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

let field_name = "include_subdirs"

type t = { mutable mode : Dune.Include_subdirs.Mode.t } [@@deriving sexp_of]

let create ~mode = { mode }
let mode t = t.mode
let set_mode t ~mode = t.mode <- mode

module Handler =
  Dunolinter.Sexp_handler.Make_sexpable
    (struct
      let field_name = field_name
    end)
    (Dune.Include_subdirs.Mode)

let read ~sexps_rewriter ~field =
  let mode = Handler.read ~sexps_rewriter ~field in
  create ~mode
;;

let write t = Handler.write t.mode
let rewrite t ~sexps_rewriter ~field = Handler.rewrite t.mode ~sexps_rewriter ~field

type predicate = Dune.Include_subdirs.Predicate.t

let eval t ~predicate =
  match (predicate : predicate) with
  | `equals mode -> Dune.Include_subdirs.Mode.equal t.mode mode |> Dunolint.Trilang.const
;;

let enforce =
  Dunolinter.Linter.enforce
    (module Dune.Include_subdirs.Predicate)
    ~eval
    ~enforce:(fun t predicate ->
      match predicate with
      | Not (`equals _) -> Eval
      | T (`equals mode) ->
        t.mode <- mode;
        Ok)
;;

module Top = struct
  type nonrec t = t

  let eval = eval
  let enforce = enforce
end

module Linter = struct
  type t = Top.t
  type predicate = Dune.Predicate.t

  let eval (t : t) ~predicate =
    (* Coverage is disabled due to many patOr, pending better bisect_ppx integration. *)
    match[@coverage off] (predicate : predicate) with
    | `stanza stanza ->
      Blang.eval stanza (fun stanza ->
        Dune.Stanza.Predicate.equal stanza `include_subdirs)
      |> Dunolint.Trilang.const
    | `include_subdirs condition ->
      Dunolint.Trilang.eval condition ~f:(fun predicate -> Top.eval t ~predicate)
    | `executable _ | `library _ | `instrumentation _ | `lint _ | `preprocess _
    | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name) ->
      Dunolint.Trilang.Undefined
  ;;

  let enforce =
    Dunolinter.Linter.enforce
      (module Dune.Predicate)
      ~eval
      ~enforce:(fun t predicate ->
        match predicate with
        | Not _ -> Eval
        | T dune ->
          (* Coverage is disabled due to many patOr, pending better bisect_ppx
             integration. *)
          (match[@coverage off] dune with
           | `include_subdirs condition ->
             Top.enforce t ~condition;
             Ok
           | `executable _
           | `stanza _
           | `library _
           | `instrumentation _
           | `lint _
           | `preprocess _
           | `has_field (`instrumentation | `lint | `name | `preprocess | `public_name) ->
             Unapplicable))
  ;;
end
