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

(** A handler for evaluating and enforcing predicates. *)

type _ Stdlib.Effect.t +=
  private
  | Enforce_failure :
      { condition : 'a
      ; sexp_of_condition : 'a -> Sexp.t
      ; loc : Loc.t
      }
      -> unit Stdlib.Effect.t
        (** A effect performed when a linter comes across a condition that is
            requested to be enforced, but there is no defined semantic for
            enforcing it automatically. Depending on the context, the course
            of actions to react to such case may vary. During linting, it may
            make sense to produce an error, and continue with the flow to
            check other rules and the other files. During a more focused
            execution, perhaps this error shall be fatal. *)

module type Predicate = sig
  type t [@@deriving sexp_of]
end

val enforce_failure
  :  (module Predicate with type t = 'a)
  -> loc:Loc.t
  -> condition:'a Blang.t
  -> unit

(** This is a special handler for the effect defined by this module,
    which will report the error, but continue the execution of linting
    and enforcing with other rules, predicates and stanzas. *)
val emit_error_and_resume : 'a -> loc:Loc.t -> f:('a -> 'b) -> 'b
