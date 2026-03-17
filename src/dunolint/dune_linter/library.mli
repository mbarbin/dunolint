(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Modes = Library__modes
module Name = Library__name
module Package = Library__package
module Public_name = Library__public_name

type t

val create
  :  ?name:Dune.Library.Name.t
  -> ?public_name:Dune.Library.Public_name.t
  -> ?package:Dune.Package.Name.t
  -> ?inline_tests:bool
  -> ?modes:Modes.Ordered_set.t
  -> ?flags:Sexp.t list
  -> ?libraries:Dune.Library.Name.t list
  -> ?libraries_to_open_via_flags:string list
  -> ?instrumentation:Instrumentation.t
  -> ?lint:Lint.t
  -> ?preprocess:Preprocess.t
  -> unit
  -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune.Library.Predicate.t

module Linter : Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t

(** {1 Getters} *)

val name : t -> Name.t option
val flags : t -> Flags.t

(** {1 Setters} *)

val set_libraries_to_open_via_flags : t -> libraries_to_open_via_flags:string list -> unit

module Private : sig end
