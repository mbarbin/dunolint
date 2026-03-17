(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Name = Executable__name
module Public_name = Executable__public_name

type t

val create
  :  ?name:Dune.Executable.Name.t
  -> ?public_name:Dune.Executable.Public_name.t
  -> ?flags:Sexp.t list
  -> ?libraries:Dune.Library.Name.t list
  -> ?instrumentation:Instrumentation.t
  -> ?lint:Lint.t
  -> ?preprocess:Preprocess.t
  -> unit
  -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune.Executable.Predicate.t

module Linter : Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t

(** {1 Getters} *)

val flags : t -> Flags.t

module Private : sig end
