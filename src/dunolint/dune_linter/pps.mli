(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The ["pps"] field indicates the preprocessor to be used. It is used as a sub
    field of the [preprocess] and [lint] fields that are found in [library],
    [executable], etc. *)

type t

module Arg : sig
  type t =
    | Pp of Dune.Pp.Name.t
    | Flag of
        { name : string
        ; param : string option
        }
  [@@deriving equal, sexp_of]
end

val create : args:Arg.t list -> t

include
  Dunolinter.Stanza_linter.S with type t := t and type predicate := Dune.Pps.Predicate.t

(** {1 Utils} *)

(** A util to create a value from a list of atoms. The [loc] is used for error
    messages, [Loc.none] may be supplied if no loc is available. *)
val parse : loc:Loc.t -> string list -> t
