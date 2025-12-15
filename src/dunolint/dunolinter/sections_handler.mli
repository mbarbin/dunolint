(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*_********************************************************************************)

(** A utility module to help with the management of fields containing separated
    sections.

    Sections denote parts that are separated by comments or blank lines within a
    field. For example, in libraries:

    {v
     (libraries
       ;; This is a first section
       aa
       bb
       ;; This is a second section
       cc
       zz)
    v}

    We make use of this module, in addition to the the [Comment_handler] to
    implement and share some functionality related to the handling of such
    sections in fields. *)

(** Tell whether two consecutive arguments are to be treated as belonging to
    different sections.

    The way dunolint does this, is to look whether two consecutive entries are
    separated by more than 1 line. In particular this covers the case where
    entries are separated by a comment in its own line, in which case dunolint
    will consider that the dependencies are in different sections.

    {v
     (libraries
       aa
       bb
       ;; this a comment
       cc
       zz)
    v}

    [are_in_different_section] must be called with two consecutive arguments,
    otherwise the returned value does not have any particular meaning. *)
val are_in_different_sections
  :  previous:Parsexp.Positions.range
  -> current:Parsexp.Positions.range
  -> bool

(** Read the arguments from an existing field, grouping them into sections
    according to the heuristic implemented by this module. *)
val read_sections_fold
  :  field_name:string
  -> sexps_rewriter:Sexps_rewriter.t
  -> field:Sexp.t
  -> init:'acc
  -> f:
       ('acc
        -> original_index:int
        -> loc:Loc.t
        -> source:string
        -> arg:Sexp.t
        -> 'acc * 'a)
  -> 'a list list

(** A simplified version of [read_sections_fold] with no accumulator. *)
val read_sections
  :  field_name:string
  -> sexps_rewriter:Sexps_rewriter.t
  -> field:Sexp.t
  -> f:(original_index:int -> loc:Loc.t -> source:string -> arg:Sexp.t -> 'a)
  -> 'a list list

(** This removes, replaces and inserts argument into [sexps_rewriter] while
    computing the diff between the input [sections] that we'd like to write, and
    the existing values found within [field] that we'd like to modify.
    Typically, [sections] is obtained from linting values according to rules,
    and [field] is the existing contents that was loaded from disk that we'd
    like to modify. *)
val rewrite_sections
  :  field_name:string
  -> sexps_rewriter:Sexps_rewriter.t
  -> field:Sexp.t
  -> write_arg:('a -> string)
  -> sections:'a list list
  -> unit
