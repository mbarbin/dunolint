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
