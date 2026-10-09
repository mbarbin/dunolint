(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type 'a t =
  | Standard
  | Element of 'a
  | Or of 'a t list
  | And of 'a t list
  | Not of 'a t
  | Diff of 'a t * 'a t
  | Unknown of Sexp.t

let rec sexp_of_t sexp_of_a : _ t -> Sexp.t = function
  | Standard -> Atom ":standard"
  | Element element -> List [ Atom "element"; sexp_of_a element ]
  | Or ts -> List (Atom "or" :: List.map ts ~f:(sexp_of_t sexp_of_a))
  | And ts -> List (Atom "and" :: List.map ts ~f:(sexp_of_t sexp_of_a))
  | Not t -> List [ Atom "not"; sexp_of_t sexp_of_a t ]
  | Diff (a, b) -> List [ Atom "diff"; sexp_of_t sexp_of_a a; sexp_of_t sexp_of_a b ]
  | Unknown sexp -> List [ Atom "unknown"; sexp ]
;;

(* Dune distinguishes quoted and unquoted atoms: only the latter may be operators or
   symbols. *)
let is_unquoted ~sexps_rewriter sexp =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  not (Char.equal original_contents.[Sexps_rewriter.start_offset sexps_rewriter sexp] '"')
;;

module Operator = struct
  type t =
    | Or
    | And
    | Not
end

module List_kind = struct
  type t =
    | Operator of Operator.t
    | Union
    | Unknown
end

(* As in dune, a list whose head is an unquoted atom other than an operator is
   reserved for future constructs, unless the atom starts with [-] or [:]. The latter
   denote a union, except for [(:include ...)]. *)
let list_kind ~sexps_rewriter (elements : Sexp.t list) : List_kind.t =
  match elements with
  | (Atom head as sexp) :: _ when is_unquoted ~sexps_rewriter sexp ->
    (match head with
     | "or" -> Operator Or
     | "and" -> Operator And
     | "not" -> Operator Not
     | ":include" -> Unknown
     | _ when String.is_prefix head ~prefix:"-" || String.is_prefix head ~prefix:":" ->
       Union
     | _ -> Unknown)
  | _ -> Union
;;

let is_backslash ~sexps_rewriter (sexp : Sexp.t) =
  match sexp with
  | Atom "\\" -> is_unquoted ~sexps_rewriter sexp
  | Atom _ | List _ -> false
;;

(* The operands on each side of the backslashes of a sequence of operands. *)
let split_at_backslashes ~sexps_rewriter operands =
  let rec loop segments current = function
    | [] -> List.rev (List.rev current :: segments)
    | operand :: operands when is_backslash ~sexps_rewriter operand ->
      loop (List.rev current :: segments) [] operands
    | operand :: operands -> loop segments (operand :: current) operands
  in
  loop [] [] operands
;;

let read ~read_element ~sexps_rewriter sexps =
  let make ~(operator : Operator.t) ts =
    match operator with
    | Or -> Or ts
    | And -> And ts
    | Not -> Not (Or ts)
  in
  let rec read_one (sexp : Sexp.t) =
    match sexp with
    | Atom atom when is_unquoted ~sexps_rewriter sexp && String.is_prefix atom ~prefix:":"
      -> if String.equal atom ":standard" then Standard else Unknown sexp
    | Atom _ -> Element (read_element ~sexps_rewriter sexp)
    | List elements ->
      (match list_kind ~sexps_rewriter elements with
       | Operator operator -> read_many (List.tl elements) ~operator
       | Unknown -> Unknown sexp
       | Union -> read_many elements ~operator:Or)
  (* A sequence of operands, combined by [operator], where an unquoted [\\] denotes a
     difference with the union of the operands that follow. *)
  and read_many operands ~(operator : Operator.t) =
    let rec loop acc (operands : Sexp.t list) =
      match operands with
      | [] -> make ~operator (List.rev acc)
      | backslash :: operands when is_backslash ~sexps_rewriter backslash ->
        Diff (make ~operator (List.rev acc), read_many operands ~operator:Or)
      | operand :: operands -> loop (read_one operand :: acc) operands
    in
    loop [] operands
  in
  read_many sexps ~operator:Or
;;

(* The rules of the canonical order, applied with [Reorder_sexp_in_place]. *)

let recurse_into ~sexps_rewriter elements =
  match list_kind ~sexps_rewriter elements with
  | Operator _ | Union -> true
  | Unknown -> false
;;

let is_fixed ~is_head (element : Sexp.t) =
  match element with
  | Atom "\\" -> true
  | Atom _ | List _ -> is_head
;;

(* The operands, as far as their order is concerned. The compound operands are compared
   by kind, then by their operands, split at the backslashes of the differences and
   sorted, so that the order doesn't depend on the order of their own operands. *)
module Operand = struct
  module Kind = struct
    (* The kinds of compound operands, in their order. *)
    type t =
      | Union
      | Or
      | And
      | Not

    let rank = function
      | Union -> 0
      | Or -> 1
      | And -> 2
      | Not -> 3
    ;;
  end

  type t =
    | Standard
    | Name of string
    | Compound of
        { kind : Kind.t
        ; segments : t list list
        }
    | Unknown

  let rank = function
    | Standard -> 0
    | Name _ -> 1
    | Compound _ -> 2
    | Unknown -> 3
  ;;

  let rec compare_lists ~compare (a : _ list) (b : _ list) : Ordering.t =
    match a, b with
    | [], [] -> Eq
    | [], _ :: _ -> Lt
    | _ :: _, [] -> Gt
    | a :: tl_a, b :: tl_b ->
      (match compare a b with
       | Ordering.Eq -> compare_lists ~compare tl_a tl_b
       | (Lt | Gt) as ordering -> ordering)
  ;;

  (* The unknown constructs compare equal, so that they keep their original order. *)
  let rec compare (a : t) (b : t) : Ordering.t =
    match a, b with
    | Name a, Name b -> Ordering.of_int (String.compare a b)
    | Compound a, Compound b ->
      (match Ordering.of_int (Int.compare (Kind.rank a.kind) (Kind.rank b.kind)) with
       | Eq -> compare_lists ~compare:(compare_lists ~compare) a.segments b.segments
       | (Lt | Gt) as ordering -> ordering)
    | (Standard | Name _ | Compound _ | Unknown), _ ->
      Ordering.of_int (Int.compare (rank a) (rank b))
  ;;

  let rec of_sexp ~sexps_rewriter (sexp : Sexp.t) =
    match sexp with
    | Atom ":standard" when is_unquoted ~sexps_rewriter sexp -> Standard
    | Atom atom -> Name atom
    | List elements ->
      let compound kind operands =
        Compound { kind; segments = segments ~sexps_rewriter operands }
      in
      (match list_kind ~sexps_rewriter elements with
       | Unknown -> Unknown
       | Union -> compound Union elements
       | Operator operator ->
         compound
           (match operator with
            | Or -> Or
            | And -> And
            | Not -> Not)
           (List.tl elements))

  and segments ~sexps_rewriter operands =
    split_at_backslashes ~sexps_rewriter operands
    |> List.map ~f:(fun segment ->
      List.map segment ~f:(of_sexp ~sexps_rewriter)
      |> List.sort ~compare:(fun a b -> Ordering.to_int (compare a b)))
  ;;
end

let compare_operands_as_sexp ~sexps_rewriter (a : Sexp.t) (b : Sexp.t) =
  Operand.compare (Operand.of_sexp ~sexps_rewriter a) (Operand.of_sexp ~sexps_rewriter b)
;;

let canonical_sort_field ~sexps_rewriter ~(field : Sexp.t) =
  (* The elements of the field are the name of the field followed by its arguments,
     which are the operands of a union. *)
  let field_elements =
    match field with
    | List elements -> Some elements
    | Atom _ -> None
  in
  Reorder_sexp_in_place.reorder
    ~sexps_rewriter
    ~sexp:field
    ~recurse_into:(fun elements ->
      Option.exists field_elements ~f:(fun field_elements ->
        phys_equal elements field_elements)
      || recurse_into ~sexps_rewriter elements)
    ~is_fixed
    ~compare:(compare_operands_as_sexp ~sexps_rewriter)
;;
