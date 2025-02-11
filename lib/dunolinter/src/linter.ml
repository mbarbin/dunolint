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

module type S = Linter_intf.S

type t =
  | Unhandled
  | T :
      { eval : Dunolint.Predicate.t -> Dunolint.Trilang.t
      ; enforce : Dunolint.Predicate.t Blang.t -> unit
      }
      -> t

let enforce_blang
      (type predicate)
      (module Predicate : Handler.Predicate with type t = predicate)
      t
      ~condition
      ~eval
      ~enforce
  =
  match (condition : _ Blang.t) with
  | Base _ -> enforce t ~condition
  | And (a, b) ->
    enforce t ~condition:a;
    enforce t ~condition:b
  | If (if_, then_, else_) ->
    (match Dunolint.Trilang.eval if_ ~f:(fun predicate -> eval t ~predicate) with
     | True -> enforce t ~condition:then_
     | False -> enforce t ~condition:else_
     | Undefined -> ())
  | True -> ()
  | (False | Not _ | Or _) as condition ->
    (match Dunolint.Trilang.eval condition ~f:(fun predicate -> eval t ~predicate) with
     | True | Undefined -> ()
     | False -> Handler.enforce_failure (module Predicate) ~loc:Loc.none ~condition)
;;

let at_positive_enforcing_position (condition : 'a Blang.t) =
  let rec loop acc t =
    match (t : _ Blang.t) with
    | Base a -> a :: acc
    | And (a, b) ->
      let acc = loop acc a in
      let acc = loop acc b in
      acc
    | If _ | True | False | Not _ | Or _ -> acc
  in
  List.rev (loop [] condition)
;;

let public_name_is_prefix name ~prefix =
  (* This helps in cases where the library public name has not the
     correct package prefix, in favoring a strategy that proposes to
     replace it by the enforced one, rather than producing a
     proposition with two dots in it. *)
  match
    if String.is_suffix prefix ~suffix:"."
    then (
      match String.lsplit2 name ~on:'.' with
      | None -> None
      | Some (_, right) -> Some (prefix ^ right))
    else None
  with
  | None -> prefix ^ name
  | Some result -> result
;;
