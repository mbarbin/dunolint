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

type t =
  | True
  | False
  | Undefined
[@@deriving equal, compare, enumerate, sexp_of]

let const = function
  | true -> True
  | false -> False
;;

let and_ a b =
  match a, b with
  | True, True -> True
  | _, False | False, _ -> False
  | (Undefined | True), Undefined | Undefined, True -> Undefined
;;

let or_ a b =
  match a, b with
  | True, (True | False | Undefined) -> True
  | (False | Undefined), True -> True
  | False, False -> False
  | (False | Undefined), Undefined -> Undefined
  | Undefined, False -> Undefined
;;

let not_ t =
  match t with
  | True -> False
  | False -> True
  | Undefined -> Undefined
;;

let exists =
  (* Returning [Undefined] doesn't shortcut, since [f] may be returning [True]
     for one of the remaining elements. *)
  let rec loop undefined_count ~f = function
    | [] -> if undefined_count > 0 then Undefined else False
    | hd :: tl ->
      (match f hd with
       | True -> True
       | Undefined -> loop (undefined_count + 1) ~f tl
       | False -> loop undefined_count ~f tl)
  in
  fun ts ~f -> loop 0 ~f ts
;;

let disjunction ts = exists ts ~f:Fn.id

let for_all =
  (* Returning [Undefined] doesn't shortcut, since [f] may be returning [False]
     for one of the remaining elements. *)
  let rec loop undefined_count ~f = function
    | [] -> if undefined_count > 0 then Undefined else True
    | hd :: tl ->
      (match f hd with
       | False -> False
       | Undefined -> loop (undefined_count + 1) ~f tl
       | True -> loop undefined_count ~f tl)
  in
  fun ts ~f -> loop 0 ~f ts
;;

let conjunction ts = for_all ts ~f:Fn.id

let rec eval (t : 'a Blang.t) ~f:base_eval : t =
  match t with
  | True -> True
  | False -> False
  | Base b -> base_eval b
  | And (t1, t2) ->
    (match eval t1 ~f:base_eval with
     | False -> False
     | (True | Undefined) as r1 ->
       (* If [r1=Undefined] and [r2=False] we should return [False] so
          we cannot skip the evaluation of r2. *)
       let r2 = eval t2 ~f:base_eval in
       and_ r1 r2)
  | Or (t1, t2) ->
    (match eval t1 ~f:base_eval with
     | True -> True
     | (False | Undefined) as r1 ->
       (* If [r1=Undefined] and [r2=True] we should return [True] so
          we cannot skip the evaluation of r2. *)
       let r2 = eval t2 ~f:base_eval in
       or_ r1 r2)
  | Not t -> not_ (eval t ~f:base_eval)
  | If (if_, th, el) ->
    (match eval if_ ~f:base_eval with
     | True -> eval th ~f:base_eval
     | False -> eval el ~f:base_eval
     | Undefined -> Undefined)
;;

module Private = struct
  let and_ = and_
  let or_ = or_
end
