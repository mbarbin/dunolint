(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Reordering the elements of the lists of a sexp, while preserving the comments of
    the original source.

    The caller supplies the rules of the language of the sexp, typically from a model of
    that language: which elements stay in place, and how the others compare. This
    module is concerned with the layout and the comments of the source.

    The source of a list is seen as a sequence of items separated by gaps. Reordering
    moves the items, while the gaps stay in place:

    - An item moves along with the comment placed on the same line, after it. When
      such an item is moved before something on the same line, a newline is inserted
      after its comment, so that the comment doesn't hide what follows.
    - Comments placed on their own line and blank lines stay in place. They delimit
      sections (see {!Sections_handler}), and items are reordered within their
      section only. Note that dune's formatter removes the blank lines of a list, thus
      in formatted files, only comments durably delimit sections: the sections
      delimited by a blank line are merged by the formatter, and reordered together
      the next time.
    - Fixed elements stay in place too, and items are not moved across them.

    The lists contained in an item are reordered too, recursively. *)

(** [reorder ~sexps_rewriter ~sexp ~recurse_into ~is_fixed ~compare] returns the source
    text of [sexp] with its lists reordered. [recurse_into elements] tells whether the
    elements of a list are reordered: when it is [false], the list is kept as in the
    source, although it may still move as an element of its parent.
    [is_fixed ~is_head element] tells whether an element of a list stays in place,
    [is_head] being [true] for the first element of the list (for example, an
    operator). The other elements are sorted according to [compare], within each run
    delimited by fixed elements and sections. The sort is stable.

    [sexp] must have been parsed by [sexps_rewriter]. *)
val reorder
  :  sexps_rewriter:Sexps_rewriter.t
  -> sexp:Sexp.t
  -> recurse_into:(Sexp.t list -> bool)
  -> is_fixed:(is_head:bool -> Sexp.t -> bool)
  -> compare:(Sexp.t -> Sexp.t -> Ordering.t)
  -> string
