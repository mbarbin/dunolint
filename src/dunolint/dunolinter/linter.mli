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
