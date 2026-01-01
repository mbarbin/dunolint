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

type 'a t =
  | Element of 'a
  | Standard
  | Union of 'a t list
  | Diff of 'a t * 'a t
  | Include of string
[@@deriving sexp_of]

let maybe_wrap_sexps = function
  | [ a ] -> a
  | ([] | _ :: _ :: _) as a -> Sexp.List a
;;

let write ~write_a t =
  let rec aux t =
    match t with
    | Element a -> [ write_a a ]
    | Standard -> [ Sexp.Atom ":standard" ]
    | Union ts -> List.concat_map ts ~f:aux
    | Diff (t1, t2) ->
      let t1 = maybe_wrap_sexps (aux t1) in
      let t2 = maybe_wrap_sexps (aux t2) in
      [ t1; Atom "\\"; t2 ]
    | Include var -> [ Atom ":include"; Atom var ]
  in
  aux t
;;

let union = function
  | [ a ] -> a
  | ([] | _ :: _ :: _) as union -> Union union
;;

let empty = Union []

let read ~read_element ~sexps_rewriter sexps =
  let rec one sexp =
    match (sexp : Sexp.t) with
    | Atom ":standard" -> Standard
    | List [ Atom ":include"; Atom var ] -> Include var
    | Atom _ -> Element (read_element ~sexps_rewriter sexp)
    | List sexps -> many [] sexps
  and many acc sexps =
    match (sexps : Sexp.t list) with
    | [] -> union (List.rev acc)
    | Atom "\\" :: tl ->
      let to_remove = many [] tl in
      Diff (union (List.rev acc), to_remove)
    | hd :: tl ->
      let x = one hd in
      many (x :: acc) tl
  in
  many [] sexps
;;

let make_union = function
  | [ t ] -> t
  | ([] | _ :: _ :: _) as ts -> Union ts
;;

let of_list elts = make_union (List.map elts ~f:(fun e -> Element e))
let of_set set = of_list (Set.to_list set)

module Evaluation_result = struct
  type 'a t =
    | Known of 'a
    | Unknown
  [@@deriving sexp_of]

  let return a = Known a

  let map a f =
    match a with
    | Unknown -> Unknown
    | Known a -> Known (f a)
  ;;

  let bind a f =
    match a with
    | Unknown -> Unknown
    | Known a -> f a
  ;;

  module Monad_syntax = struct
    let return = return
    let ( let+ ) = map
    let ( let* ) = bind
  end
end

module Evaluator = struct
  type 'a t =
    { standard : unit -> 'a list Evaluation_result.t
    ; include_ : string -> 'a list Evaluation_result.t
    }

  let static = { standard = (fun () -> Unknown); include_ = (fun _ -> Unknown) }
end

let as_set (m : _ Comparator.Module.t) t ~(evaluator : _ Evaluator.t) =
  let open Evaluation_result.Monad_syntax in
  let rec aux t =
    match (t : _ t) with
    | Element a -> return (Set.singleton m a)
    | Standard ->
      let+ list = evaluator.standard () in
      Set.of_list m list
    | Union ts ->
      let rec loop acc = function
        | [] -> return (List.rev acc)
        | hd :: tl ->
          let* hd = aux hd in
          loop (hd :: acc) tl
      in
      let+ ts = loop [] ts in
      Set.union_list m ts
    | Diff (a, b) ->
      let* a = aux a in
      let+ b = aux b in
      Set.diff a b
    | Include var ->
      let+ list = evaluator.include_ var in
      Set.of_list m list
  in
  aux t
;;

let mem
      (type a)
      (module M : Comparator.S with type t = a)
      t
      elt
      ~(evaluator : _ Evaluator.t)
  =
  let open Evaluation_result.Monad_syntax in
  let compare = M.comparator.compare in
  let equal a b = compare a b = 0 in
  let rec aux t =
    match (t : _ t) with
    | Element a -> return (equal a elt)
    | Standard ->
      let+ list = evaluator.standard () in
      List.mem list elt ~equal
    | Union ts ->
      let ts = List.map ts ~f:aux in
      let rec loop undefined = function
        | [] -> if undefined then Evaluation_result.Unknown else return false
        | hd :: tl ->
          (match (hd : _ Evaluation_result.t) with
           | Known true -> return true
           | Known false -> loop undefined tl
           | Unknown -> loop true tl)
      in
      loop false ts
    | Diff (a, b) ->
      (match aux a, aux b with
       | Known a, Known b -> return (a && not b)
       | Unknown, Known b -> if b then return false else Unknown
       | Known a, Unknown -> if not a then return false else Unknown
       | Unknown, Unknown -> Unknown)
    | Include var ->
      let+ list = evaluator.include_ var in
      List.mem list elt ~equal
  in
  aux t
;;

let insert (type a) (module M : Comparator.S with type t = a) t elt =
  let compare = M.comparator.compare in
  let rec aux t =
    match (t : _ t) with
    | Element a ->
      (match compare a elt |> Ordering.of_int with
       | Less -> Union [ t; Element elt ]
       | Equal -> t
       | Greater -> Union [ Element elt; t ])
    | Standard | Include _ -> Union [ t; Element elt ]
    | Diff (a, b) -> Diff (aux a, b)
    | Union ts ->
      let rec loop acc ts =
        match ts with
        | [] -> List.rev (Element elt :: acc)
        | hd :: tl ->
          (match hd with
           | Standard | Include _ | Diff _ -> loop (hd :: acc) tl
           | Union _ -> List.rev_append (aux hd :: acc) tl
           | Element a ->
             (match compare a elt |> Ordering.of_int with
              | Less -> loop (hd :: acc) tl
              | Equal -> List.rev_append acc ts
              | Greater -> List.rev_append (Element elt :: acc) ts))
      in
      make_union (loop [] ts)
  in
  aux t
;;

let remove (type a) (module M : Comparator.S with type t = a) t elt =
  let compare = M.comparator.compare in
  let rec aux t =
    match (t : _ t) with
    | Element a ->
      (match compare a elt |> Ordering.of_int with
       | Less | Greater -> t
       | Equal -> Union [])
    | Standard | Include _ -> t
    | Diff (a, b) -> Diff (aux a, b)
    | Union ts ->
      let rec loop acc ts =
        match ts with
        | [] -> List.rev acc
        | hd :: tl ->
          (match hd with
           | Standard | Include _ | Diff _ | Union _ -> loop (aux hd :: acc) tl
           | Element a ->
             (match compare a elt |> Ordering.of_int with
              | Less | Greater -> loop (hd :: acc) tl
              | Equal -> loop acc tl))
      in
      make_union (loop [] ts)
  in
  aux t
;;

let top_constructor_sort_value = function
  | Include _ -> 0
  | Standard -> 1
  | Union _ -> 2
  | Diff _ -> 3
  | Element _ -> 4
;;

let canonical_compare a b ~compare =
  match a, b with
  | Element a, Element b -> compare a b
  | _, _ -> Int.compare (top_constructor_sort_value a) (top_constructor_sort_value b)
;;

let canonical_sort (type a) (module M : Comparator.S with type t = a) t =
  let compare = M.comparator.compare in
  let rec aux t =
    match t with
    | (Element _ | Standard | Include _) as t -> t
    | Union ts ->
      Union (List.sort ts ~compare:(fun a b -> canonical_compare a b ~compare))
    | Diff (a, b) -> Diff (aux a, aux b)
  in
  aux t
;;
