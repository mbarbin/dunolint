(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A model of dune's predicate language, as used for example by the [dirs] stanza.

    This follows dune's implementation (see [src/predicate_lang/predicate_lang.ml] in
    dune's repository), where the elements are globs:

    {v
      pred : (or pred ...)
           : (and pred ...)
           : (not pred ...)    ; The negation of the union of the operands.
           : :standard
           : element
           : (pred ...)        ; The union of the operands.
    v}

    In a sequence of operands, a backslash denotes a difference: the union of the
    operands that precede it, minus the union of the ones that follow it.

    This is not to be confused with dune's ordered set language (see {!Ordered_set}),
    which has no boolean operators. *)

type 'a t =
  | Standard (** [:standard]. *)
  | Element of 'a
  | Or of 'a t list
  (** [(or a b ...)], as well as the operands of a list without operator, and of
      the arguments of a field. *)
  | And of 'a t list (** [(and a b ...)]. *)
  | Not of 'a t
  (** [(not a b ...)] is the negation of the union of its operands, thus it is
      represented as [Not (Or [ a; b; ... ])]. *)
  | Diff of 'a t * 'a t
  (** In a sequence of operands, a backslash denotes the difference between the
      operands that precede it, combined as by the enclosing construct, and the union
      of the ones that follow it. For example, [(and a b \\ c d)] is represented as
      [Diff (And [ a; b ], Or [ c; d ])]. The operands that follow may contain a
      backslash too, thus [a \\ b \\ c] is represented as
      [Diff (Or [ a ], Diff (Or [ b ], Or [ c ]))]. *)
  | Unknown of Sexp.t
  (** A construct dunolint doesn't know: a symbol starting with [:] other than
      [:standard], [(:include ...)], or a list whose head is an unquoted atom other
      than [or], [and] and [not], which dune reserves for future constructs. They're
      kept as they are, so that dunolint doesn't need to know about the constructs
      added to dune, or rejected by it. *)

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

(** Read the arguments of a field, such as [(dirs ...)], reading the elements, which
    are atoms, with [read_element]. This follows dune in telling quoted and unquoted
    atoms apart: only the latter may be operators or symbols. The constructs that
    dunolint doesn't know are read as [Unknown]. *)
val read
  :  read_element:(sexps_rewriter:Sexps_rewriter.t -> Sexp.t -> 'a)
  -> sexps_rewriter:Sexps_rewriter.t
  -> Sexp.t list
  -> 'a t

(** {1 Canonical order} *)

(** [canonical_sort_field ~sexps_rewriter ~field] returns the source text of [field],
    with its arguments sorted in the canonical order of dunolint, keeping the comments
    of the source (see {!Reorder_sexp_in_place}). The arguments of [field] are the
    operands of a union, as in the [dirs] stanza.

    The operands of unions, and of [or], [and] and [not] commute, and are sorted:
    [:standard] first, then the elements in alphabetical order, then the compound
    operands: the unions written as lists without operator, then the [or], the [and]
    and the [not]. Compound operands of the same kind are compared by their own
    operands, sorted the same way, so that their order doesn't depend on how they are
    written. The [Unknown] constructs come last, in their original order, and their
    operands aren't reordered, since they may not commute.

    The head of a list and the backslash of a difference stay in place, and operands
    are not moved across a backslash. The head of a list is either an operator, or the
    first operand of a union, which dune requires to be quoted unless it starts with
    [-] or [:]: moving it would require to change the quoting of the operands, which is
    not done.

    [field] must have been parsed by [sexps_rewriter]. *)
val canonical_sort_field : sexps_rewriter:Sexps_rewriter.t -> field:Sexp.t -> string
