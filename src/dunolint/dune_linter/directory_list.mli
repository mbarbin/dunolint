(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A stanza whose arguments are a plain list of directory names or globs, such as
    [(data_only_dirs ...)] or [(vendored_dirs ...)].

    There are no predicates for such stanzas. Their rewrite applies the canonical
    ordering of dunolint: the directories are sorted, keeping the comments of the
    original source. *)

module type S = sig
  type t

  include Dunolinter.Stanza_linter.S with type t := t and type predicate = Nothing.t

  module Linter :
    Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t
end

module Make (_ : sig
    val field_name : string
  end) : S
