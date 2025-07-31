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

module Predicate = struct
  type 'a t =
    | T of 'a
    | Not of 'a

  let to_blang = function
    | T a -> Blang.base a
    | Not a -> Blang.not_ (Blang.base a)
  ;;
end

let enforce
      (type predicate)
      (module Handler_predicate : Handler.Predicate with type t = predicate)
      ~eval
      ~(enforce : 't -> predicate Predicate.t -> Enforce_result.t)
  =
  let check t ~condition =
    match Dunolint.Trilang.eval condition ~f:(fun predicate -> eval t ~predicate) with
    | True | Undefined -> ()
    | False -> Handler.enforce_failure (module Handler_predicate) ~loc:Loc.none ~condition
  in
  let enforce t predicate =
    match enforce t predicate with
    | Ok | Unapplicable -> ()
    | Eval -> check t ~condition:(Predicate.to_blang predicate)
    | Fail ->
      Handler.enforce_failure
        (module Handler_predicate)
        ~loc:Loc.none
        ~condition:(Predicate.to_blang predicate)
  in
  let rec aux t ~condition =
    match (condition : predicate Blang.t) with
    | Base predicate -> enforce t (T predicate)
    | Not (Base predicate) -> enforce t (Not predicate)
    | And (a, b) ->
      aux t ~condition:a;
      aux t ~condition:b
    | If (if_, then_, else_) ->
      (match Dunolint.Trilang.eval if_ ~f:(fun predicate -> eval t ~predicate) with
       | True -> aux t ~condition:then_
       | False -> aux t ~condition:else_
       | Undefined -> ())
    | (True | False | Not _ | Or _) as condition -> check t ~condition
  in
  aux
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
  (* This helps in cases where the library public name has not the correct
     package prefix, in favoring a strategy that proposes to replace it by the
     enforced one, rather than producing a proposition with two dots in it. *)
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
