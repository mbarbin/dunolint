(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val create : dunolint_lang_version:Dunolint0.Dunolint_lang_version.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dunolint0.Dunolint_lang_version.Predicate.t

module Linter :
  Dunolinter.Linter.S with type t = t and type predicate = Dunolint0.Predicate.t

(** {1 Getters} *)

val dunolint_lang_version : t -> Dunolint0.Dunolint_lang_version.t

(** {1 Setters} *)

val set_dunolint_lang_version
  :  t
  -> dunolint_lang_version:Dunolint0.Dunolint_lang_version.t
  -> unit
