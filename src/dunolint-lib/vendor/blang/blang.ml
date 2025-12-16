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

(*_ Notice: This file was vendored from [Core], which we documented in the NOTICE
  at the root of the repo.

  The original license header was kept with the file, see below.

  List of changes:

  - Remove part of the module that is not required by the project.

  - Remove dependency to ppx.
*)

(* The MIT License

   Copyright (c) 2008--2024 Jane Street Group, LLC
   <opensource-contacts@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

module List = ListLabels

(* The module [T] serves to enforce the invariant that all Blang.t values are in
   a normal form whereby boolean constants True and False only appear as the
   topmost constructor -- in any other position they are simplified away using
   laws of boolean algebra.

   We also enforce that nested [And]s and [Or]s each lean to the right so that
   [eval] doesn't need so much stack space as it would if they leaned to the
   left. Thought experiment: compare how [eval] works on right-leaning
   [And (a, And (b, And (c, d)))] versus left-leaning
   [And (And (And (a, b), c), d)]. The former is the best case and is enforced.

   Note: this file deviates from the usual pattern of modules with Stable
   interfaces in that the Stable sub-module is not the first thing to be defined
   in the module. The reason for this deviation is so that one can convince
   oneself of the aforementioned invariant after reading only this small amount
   of code. After defining T we then immediately define its Stable interface. *)
module T : sig
  type +'a t = private
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val true_ : 'a t
  val false_ : 'a t
  val not_ : 'a t -> 'a t
  val andalso : 'a t -> 'a t -> 'a t
  val orelse : 'a t -> 'a t -> 'a t
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
  val base : 'a -> 'a t
end = struct
  type +'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a

  let rec compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a ->
    fun a__001_ ->
    fun b__002_ ->
    if Stdlib.( == ) a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | True, True -> 0
      | True, _ -> -1
      | _, True -> 1
      | False, False -> 0
      | False, _ -> -1
      | _, False -> 1
      | And (_a__003_, _a__005_), And (_b__004_, _b__006_) ->
        (match compare _cmp__a _a__003_ _b__004_ with
         | 0 -> compare _cmp__a _a__005_ _b__006_
         | n -> n)
      | And _, _ -> -1
      | _, And _ -> 1
      | Or (_a__011_, _a__013_), Or (_b__012_, _b__014_) ->
        (match compare _cmp__a _a__011_ _b__012_ with
         | 0 -> compare _cmp__a _a__013_ _b__014_
         | n -> n)
      | Or _, _ -> -1
      | _, Or _ -> 1
      | Not _a__019_, Not _b__020_ -> compare _cmp__a _a__019_ _b__020_
      | Not _, _ -> -1
      | _, Not _ -> 1
      | If (_a__023_, _a__025_, _a__027_), If (_b__024_, _b__026_, _b__028_) ->
        (match compare _cmp__a _a__023_ _b__024_ with
         | 0 ->
           (match compare _cmp__a _a__025_ _b__026_ with
            | 0 -> compare _cmp__a _a__027_ _b__028_
            | n -> n)
         | n -> n)
      | If _, _ -> -1
      | _, If _ -> 1
      | Base _a__035_, Base _b__036_ -> _cmp__a _a__035_ _b__036_)
  ;;

  let rec equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a ->
    fun a__037_ ->
    fun b__038_ ->
    if Stdlib.( == ) a__037_ b__038_
    then true
    else (
      match a__037_, b__038_ with
      | True, True -> true
      | True, _ -> false
      | _, True -> false
      | False, False -> true
      | False, _ -> false
      | _, False -> false
      | And (_a__039_, _a__041_), And (_b__040_, _b__042_) ->
        Stdlib.( && ) (equal _cmp__a _a__039_ _b__040_) (equal _cmp__a _a__041_ _b__042_)
      | And _, _ -> false
      | _, And _ -> false
      | Or (_a__047_, _a__049_), Or (_b__048_, _b__050_) ->
        Stdlib.( && ) (equal _cmp__a _a__047_ _b__048_) (equal _cmp__a _a__049_ _b__050_)
      | Or _, _ -> false
      | _, Or _ -> false
      | Not _a__055_, Not _b__056_ -> equal _cmp__a _a__055_ _b__056_
      | Not _, _ -> false
      | _, Not _ -> false
      | If (_a__059_, _a__061_, _a__063_), If (_b__060_, _b__062_, _b__064_) ->
        Stdlib.( && )
          (equal _cmp__a _a__059_ _b__060_)
          (Stdlib.( && )
             (equal _cmp__a _a__061_ _b__062_)
             (equal _cmp__a _a__063_ _b__064_))
      | If _, _ -> false
      | _, If _ -> false
      | Base _a__071_, Base _b__072_ -> _cmp__a _a__071_ _b__072_)
  ;;

  let true_ = True
  let false_ = False
  let base v = Base v

  let not_ = function
    | True -> False
    | False -> True
    | Not t -> t
    | t -> Not t
  ;;

  let rec andalso t1 t2 =
    match t1, t2 with
    | _, False | False, _ -> False
    | other, True | True, other -> other
    | And (t1a, t1b), _ ->
      (* nested [And]s lean right -- see comment above *)
      And (t1a, andalso t1b t2)
    | _ -> And (t1, t2)
  ;;

  let rec orelse t1 t2 =
    match t1, t2 with
    | _, True | True, _ -> True
    | other, False | False, other -> other
    | Or (t1a, t1b), _ ->
      (* nested [Or]s lean right -- see comment above *)
      Or (t1a, orelse t1b t2)
    | _ -> Or (t1, t2)
  ;;

  let if_ a b c =
    match a with
    | True -> b
    | False -> c
    | _ ->
      (match b, c with
       | True, _ -> orelse a c
       | _, False -> andalso a b
       | _, True -> orelse (not_ a) b
       | False, _ -> andalso (not_ a) c
       | _ -> If (a, b, c))
  ;;
end

include T

module Stable = struct
  module V1 : sig
    (* THIS TYPE AND ITS SERIALIZATIONS SHOULD NEVER BE CHANGED - PLEASE SPEAK
       WITH ANOTHER DEVELOPER IF YOU NEED MORE DETAIL *)

    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    include Sexpable.S1 with type 'a t := 'a t

    (* the remainder of this signature consists of functions used in the
       definitions of sexp conversions that are also useful more generally *)

    val and_ : 'a t list -> 'a t
    val or_ : 'a t list -> 'a t
    val gather_conjuncts : 'a t -> 'a t list
    val gather_disjuncts : 'a t -> 'a t list
  end = struct
    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a

    include (
      T :
        sig
          type 'a t

          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        end
        with type 'a t := 'a t)

    type sexp = Sexp.t =
      | Atom of string
      | List of sexp list

    (* cheap import *)

    (* flatten out nested and's *)
    let gather_conjuncts t =
      let rec loop acc = function
        | True :: ts -> loop acc ts
        | And (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t :: ts -> loop (t :: acc) ts
        | [] -> List.rev acc
      in
      loop [] [ t ]
    ;;

    (* flatten out nested or's *)
    let gather_disjuncts t =
      let rec loop acc = function
        | False :: ts -> loop acc ts
        | Or (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t :: ts -> loop (t :: acc) ts
        | [] -> List.rev acc
      in
      loop [] [ t ]
    ;;

    (* [and_] and [or_] use [fold_right] instead of [fold_left] to avoid
       quadratic behavior with [andalso] or [orelse], respectively. *)
    let and_ ts = List.fold_right ts ~init:true_ ~f:andalso
    let or_ ts = List.fold_right ts ~init:false_ ~f:orelse
    let of_sexp_error str sexp = raise (Sexp.Of_sexp_error (Failure str, sexp))

    let unary name args sexp =
      match args with
      | [ x ] -> x
      | _ ->
        let n = List.length args in
        of_sexp_error (Printf.sprintf "%s expects one argument, %d found" name n) sexp
    ;;

    let ternary name args sexp =
      match args with
      | [ x; y; z ] -> x, y, z
      | _ ->
        let n = List.length args in
        of_sexp_error (Printf.sprintf "%s expects three arguments, %d found" name n) sexp
    ;;

    let sexp_of_t sexp_of_value t =
      let rec aux t =
        match t with
        | Base x -> sexp_of_value x
        | True -> Atom "true"
        | False -> Atom "false"
        | Not t -> List [ Atom "not"; aux t ]
        | If (t1, t2, t3) -> List [ Atom "if"; aux t1; aux t2; aux t3 ]
        | And _ as t ->
          let ts = gather_conjuncts t in
          List (Atom "and" :: List.map ~f:aux ts)
        | Or _ as t ->
          let ts = gather_disjuncts t in
          List (Atom "or" :: List.map ~f:aux ts)
      in
      aux t
    ;;

    let t_of_sexp base_of_sexp sexp =
      let base sexp = base (base_of_sexp sexp) in
      let rec aux sexp =
        match sexp with
        | Atom kw ->
          (match String.lowercase_ascii kw with
           | "true" -> true_
           | "false" -> false_
           | _ -> base sexp)
        | List (Atom kw :: args) ->
          (match String.lowercase_ascii kw with
           | "and" -> and_ (List.map ~f:aux args)
           | "or" -> or_ (List.map ~f:aux args)
           | "not" -> not_ (aux (unary "not" args sexp))
           | "if" ->
             let x, y, z = ternary "if" args sexp in
             if_ (aux x) (aux y) (aux z)
           | _ -> base sexp)
        | _ -> base sexp
      in
      aux sexp
    ;;
  end
end

include (Stable.V1 : module type of Stable.V1 with type 'a t := 'a t)

let constant b = if b then true_ else false_

module type Constructors = sig
  val base : 'a -> 'a t
  val true_ : _ t
  val false_ : _ t
  val constant : bool -> _ t
  val not_ : 'a t -> 'a t
  val and_ : 'a t list -> 'a t
  val or_ : 'a t list -> 'a t
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
end

module O = struct
  include T

  let not = not_
  let and_ = and_
  let or_ = or_
  let constant = constant
  let ( && ) = andalso
  let ( || ) = orelse
  let ( ==> ) a b = (not a) || b
end

(* semantics *)

let rec eval t base_eval =
  match t with
  | True -> true
  | False -> false
  | And (t1, t2) -> eval t1 base_eval && eval t2 base_eval
  | Or (t1, t2) -> eval t1 base_eval || eval t2 base_eval
  | Not t -> not (eval t base_eval)
  | If (t1, t2, t3) -> if eval t1 base_eval then eval t2 base_eval else eval t3 base_eval
  | Base x -> base_eval x
;;
