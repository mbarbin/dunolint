(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* Some functions are copied from [Base] version [v0.17] which is released
   under MIT and may be found at [https://github.com/janestreet/base].

   See Base's LICENSE below:

   ----------------------------------------------------------------------------

   The MIT License

   Copyright (c) 2016--2024 Jane Street Group, LLC <opensource-contacts@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   ----------------------------------------------------------------------------

   When this is the case, we clearly indicate it next to the copied function. *)

include Stdlib.ListLabels

let concat_map t ~f = concat_map ~f t
let fold t ~init ~f = fold_left ~f ~init t
let bind = concat_map
let count t ~f = fold t ~init:0 ~f:(fun acc x -> if f x then acc + 1 else acc)
let sort l ~compare = Stdlib.ListLabels.sort ~cmp:compare l

let dedup_and_sort t ~compare =
  let sorted = sort t ~compare in
  let rec dedup acc = function
    | [] -> rev acc
    | [ x ] -> rev (x :: acc)
    | x :: (y :: _ as rest) ->
      if compare x y = 0 then dedup acc rest else dedup (x :: acc) rest
  in
  dedup [] sorted
;;

let equal eq a b = equal ~eq a b
let exists t ~f = exists ~f t
let filter t ~f = filter ~f t
let filter_opt t = filter_map ~f:Fun.id t
let find t ~f = find_opt ~f t
let find_map t ~f = find_map ~f t
let for_all t ~f = for_all ~f t
let iter t ~f = iter ~f t
let iter2 a b ~f = iter2 ~f a b
let iteri t ~f = iteri ~f t

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: (_ :: _ as tail) -> last tail
;;

let rec last_exn = function
  | [] -> failwith "List.last_exn"
  | [ last ] -> last
  | _ :: (_ :: _ as tail) -> last_exn tail
;;

let map t ~f = map ~f t

let rev_filter_map t ~f =
  let rec aux t ~f acc =
    match t with
    | [] -> acc
    | hd :: tl ->
      let acc =
        match f hd with
        | Some x -> x :: acc
        | None -> acc
      in
      aux tl ~f acc
  in
  aux t ~f []
;;

let take t s = take s t

let max_elt t ~compare =
  match t with
  | [] -> None
  | hd :: tl ->
    Some
      (fold tl ~init:hd ~f:(fun acc x ->
         match compare x acc |> Ordering.of_int with
         | Gt -> x
         | Lt | Eq -> acc))
;;

let mem t x ~equal = exists t ~f:(fun y -> equal x y)

(* ---------------------------------------------------------------------------- *)
(* [Summable] and [sum] are copied from [Base] (MIT). See notice at the top of
   the file and project global notice for licensing information. *)

module type Summable = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
end

let sum (type a) (module M : Summable with type t = a) t ~f =
  fold t ~init:M.zero ~f:(fun acc x -> M.( + ) acc (f x))
;;

(* ---------------------------------------------------------------------------- *)
(* [fold_map], [fold_mapi], [groupi] and [group] are copied from [Base] (MIT).
   See notice at the top of the file and project global notice for licensing
   information. *)

let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let result =
    mapi t ~f:(fun i x ->
      let new_acc, y = f i !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let groupi l ~break =
  (* We allocate shared position and list references so we can make the inner loop use
     [[@tail_mod_cons]], and still return back information about position and where in the
     list we left off. *)
  let pos = ref 0 in
  let l = ref l in
  (* As a result of using local references, our inner loop does not need arguments. *)
  let[@tail_mod_cons] rec take_group () =
    match !l with
    | [] -> assert false
    | [ _ ] as group ->
      l := [];
      group
    | x :: (y :: _ as tl) ->
      pos := !pos + 1;
      l := tl;
      if break !pos x y
      then [ x ]
      else
        (* Coverage is off in the second part of the expression because the
           instrumentation breaks [@tail_mod_cons], triggering warning 71. *)
        x :: (take_group () [@coverage off])
  in
  (* Our outer loop does not need arguments, either. *)
  let[@tail_mod_cons] rec groups () =
    if is_empty !l
    then []
    else (
      let group = take_group () in
      (* Coverage is off in the second part of the expression because the
         instrumentation breaks [@tail_mod_cons], triggering warning 71. *)
      group :: (groups () [@coverage off]))
  in
  (groups () [@nontail])
;;

let group l ~break = groupi l ~break:(fun _ x y -> break x y) [@nontail]
(* ---------------------------------------------------------------------------- *)
