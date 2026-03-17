(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The ["instrumentation"] field indicates the instrumentation to be used. It
    is used in stanza such as [library], [executable], etc. *)

type t

val create : backend:Dune.Instrumentation.Backend.t -> t

(** When the field appears in the condition blang, we create a first value to
    initialize when the field is not originally present. *)
val initialize : condition:Dune.Instrumentation.Predicate.t Blang.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate := Dune.Instrumentation.Predicate.t

(** {1 Getters} *)

val backend : t -> Dune.Instrumentation.Backend.t

(** {1 Setters} *)

val set_backend : t -> backend:Dune.Instrumentation.Backend.t -> unit
