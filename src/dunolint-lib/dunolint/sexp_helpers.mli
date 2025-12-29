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

(** Some helpers used by sexp serializers.

    Originally the sexp reader in this directory were implemented using ppx meta
    programming via [ppx_sexp_conv] however we are in the process of migrating
    the logic to custom helpers in order to:

    1. remove dependencies
    2. allow customization (e.g. less parens required for variants like in dune)
    3. improve error handling and reporting

    This module captures some common patterns which we are gradually introducing
    to the code to handle to sexp handling. *)

module type T_of_sexp = sig
  type t

  val t_of_sexp : Sexp.t -> t
end

(** When a record is embedded by a variant or polymorphic variant we'd like to
    support a syntax with less parens around. For example:

    Suppose you have a record type M:

    {[
      module M = struct
        type t =
          { a : string
          ; b : int
          }
      end

      type t = [ `cons of M.t ]
    ]}

    We'd like to parse:

    {[
      cons (a hello) (b 42)
    ]}

    Instead of:

    {[
      cons ((a hello) (b 42))
    ]}

    However care must be applied for the parsing exceptions raised by use an
    actual sexp of the input, otherwise there would be no location. [context] is
    the sexp used to error out, and [fields] the record fields. M is able to
    parse the fields when they are wrapped by a [Sexp.List] constructor. *)
val parse_inline_record
  :  (module T_of_sexp with type t = 'a)
  -> error_source:string
  -> context:Sexp.t
  -> tag:string
  -> fields:Sexp.t list
  -> 'a

module Variant_spec : sig
  (** Helper to read variants from s-expressions. Supports nullary, unary, and
      variadic variants with proper error messages for each case.

      The goal overtime is to extend the capability of this mini interpreter by
      improving behaviors such as error reporting, user-friendly hints, etc.
      This is left as future work. *)

  type 'a conv =
    | Nullary of 'a (** Variant with no argument, e.g., [`return] *)
    | Unary_with_context of (context:Sexp.t -> arg:Sexp.t -> 'a)
    (** Variant with one argument, e.g., [`equals of string] *)
    | Unary of (Sexp.t -> 'a) (** For Unary when context is not needed (most of them). *)
    | Variadic of (context:Sexp.t -> fields:Sexp.t list -> 'a)
    (** Variant with multiple arguments, e.g., [`skip_paths of Glob.t list] *)

  type 'a case =
    { atom : string
    ; conv : 'a conv
    }

  type 'a t = 'a case list
end

val parse_variant : 'a Variant_spec.t -> error_source:string -> Sexp.t -> 'a
