(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

(** This is a handler for the effect defined by this module, which will
    discontinue the effect with an exception. It is suitable for use in expect
    tests. *)
val raise : f:(unit -> 'a) -> 'a
