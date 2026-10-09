(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The [dirs] stanza, which specifies the subdirectories to include in the build,
    using dune's predicate language (see {!Dunolinter.Predicate_lang}), such as
    [(dirs :standard \\ foo bar)].

    There are no predicates for this stanza. Its rewrite applies the canonical
    ordering of dunolint: the operands of the unions and of the [or], [and] and
    [not] operators are sorted, keeping the comments of the original source. *)

type t

include Dunolinter.Stanza_linter.S with type t := t and type predicate = Nothing.t
module Linter : Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t
