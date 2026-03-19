(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

(** {1 Error handling} *)

module Error_context : sig
  (** Embed context to make parsing errors more user-friendly.

      We're using a custom exception that allows embedding special context whose
      purpose is to improve the parsing errors that dunolint prints to the
      users.

      For example, when we fail to find a match for a particular construct or
      keyword looked up by name, we give the list of known ones so as to allow
      messages like: "Did you mean X?".

      The way we integrate this with the raising mecanism of [Sexplib0] is to
      embed the context into the exception type defined below, and have such
      exception be the first argument to the [Of_sexp_error (e, _)] error. *)

  type t

  exception E of t

  val message : t -> string

  module Did_you_mean : sig
    type t =
      { var : string
      ; candidates : string list
      }
  end

  val did_you_mean : t -> Did_you_mean.t option
  val suggestion : t -> string option
end

(** {1 Parsing utils} *)

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
