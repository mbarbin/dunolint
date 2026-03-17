(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A type to describe the result of an attempt to enforce a predicate during
    linting. *)

type t =
  | Ok
  (** The enforcement of the predicate was successful, or perhaps the required
      condition was already verified without requiring to perform any
      change. *)
  | Fail
  (** The enforcement of such predicate cannot succeed and requires the user's
      intervention. *)
  | Eval
  (** This is a special value that instructs the call site to check with
      [eval] whether the required condition holds. If the evaluation returns
      [True], the end result is the same as for an [Ok] status. If the
      evaluation returns [False], this will result in a [Fail]. If the
      evaluation is [Undefined], this results in an [Unapplicable] status. *)
  | Unapplicable
  (** The predicate in question does not apply to the stanza currently at
      hand. For example, it starts with a selector that does not match the
      stanza being linted. Unapplicable predicates are ignored by dunolint. *)
