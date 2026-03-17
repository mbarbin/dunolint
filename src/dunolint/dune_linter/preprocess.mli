(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The ["preprocess"] field indicates the preprocessor to be used. It is used
    in stanza such as [library], [executable], etc. *)

type t

val create : ?pps:Pps.t -> unit -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate := Dune.Preprocess.Predicate.t

(** {1 Getters} *)

module State : sig
  type t =
    | No_preprocessing
    | Pps of Pps.t
    | Unhandled of Sexp.t
  [@@deriving sexp_of]
end

val state : t -> State.t

(** {1 Setters} *)

val set_state : t -> state:State.t -> unit
