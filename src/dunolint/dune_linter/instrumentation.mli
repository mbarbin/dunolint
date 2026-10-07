(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The ["instrumentation"] field indicates the instrumentation to be used. It
    is used in stanza such as [library], [executable], etc. *)

type t

val create : backend:Dune.Instrumentation.Backend.t -> t

(** A shallow copy, used to enforce conditions on a copy of the entries. *)
val copy : t -> t

val sexp_of_t : t -> Sexp.t

(** The conditions of stanzas about their instrumentation fields are evaluated and
    enforced by [Instrumentation_entries]. *)
include Dunolinter.Sexp_handler.S with type t := t

(** {1 Getters} *)

val backend : t -> Dune.Instrumentation.Backend.t
val has_backend_name : t -> name:Dune.Instrumentation.Backend.Name.t -> bool

(** {1 Setters} *)

val set_backend : t -> backend:Dune.Instrumentation.Backend.t -> unit
