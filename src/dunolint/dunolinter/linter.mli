(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module type S = Linter_intf.S

type t =
  | Unhandled
  | T :
      { eval :
          path:Relative_path.t -> predicate:Dunolint.Predicate.t -> Dunolint.Trilang.t
      ; enforce : path:Relative_path.t -> condition:Dunolint.Predicate.t Blang.t -> unit
      }
      -> t

(** {1 Helpers} *)

module Predicate : sig
  type 'a t =
    | T of 'a
    | Not of 'a
end

(** A helper function that can be useful to implement the [enforce] function
    required by the [Linter.S] interface. *)
val enforce
  :  (module Handler.Predicate with type t = 'predicate)
  -> eval:('t -> predicate:'predicate -> Dunolint.Trilang.t)
  -> enforce:('t -> 'predicate Predicate.t -> Enforce_result.t)
  -> 't
  -> condition:'predicate Blang.t
  -> unit

(** Search for an initial value that can be used to initialize an absent field.

    Only predicates at positive enforcing positions (Base and And) are
    considered. The function [f] should return [Some value] for predicates
    that can provide a concrete initial value (like [equals]), and [None]
    for predicates that cannot (like [is_prefix] or [is_suffix]).

    Returns the first matching value found, or [None] if no predicate can
    provide an initial value. *)
val find_init_value : 'a Blang.t -> f:('a -> 'b option) -> 'b option

(** A helper that applies some usually helpful heuristic when proposing a new
    name based on the [`is_prefix] predicate. Assumed to be called when the
    given prefix is not already a prefix of the input, otherwise the output is
    unspecified. *)
val public_name_is_prefix : string -> prefix:string -> string
