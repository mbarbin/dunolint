(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val create : dune_lang_version:Dune_workspace.Dune_lang_version.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune_workspace.Dune_lang_version.Predicate.t

module Linter :
  Dunolinter.Linter.S with type t = t and type predicate = Dune_workspace.Predicate.t

(** {1 Getters} *)

val dune_lang_version : t -> Dune_workspace.Dune_lang_version.t

(** {1 Setters} *)

val set_dune_lang_version
  :  t
  -> dune_lang_version:Dune_workspace.Dune_lang_version.t
  -> unit
