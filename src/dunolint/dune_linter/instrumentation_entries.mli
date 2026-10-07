(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Shared utils for supporting stanzas with several instrumentation fields.

    This is a mutable container holding the instrumentation entries of a stanza. *)

type t

val create : Instrumentation.t list -> t
val to_list : t -> Instrumentation.t list
val is_empty : t -> bool

(** Write the entries as a list of [instrumentation] fields. *)
val write : t -> Sexp.t list

(** Remove all entries. *)
val clear : t -> unit

(** Add an entry with the default backend if there are none. *)
val initialize_if_empty : t -> unit

(** In the sexp arguments of the [instrumentation] field locate a valid backend
    name. *)
val find_instrumentation_backend
  :  Sexp.t list
  -> Dune.Instrumentation.Backend.Name.t option

(** Helper to be used along with [Dunolinter.Sexps_handler.insert_new_fields].
    Returns [true] iif the args mention the same instrumentation backend. *)
val insertion_overlaps : present_args:Sexp.t list -> new_args:Sexp.t list -> bool

(** A field without a matching entry is left untouched, unless the field is marked for
    removal. *)
val rewrite
  :  t
  -> args:Sexp.t list
  -> marked_for_removal:bool
  -> [ `Keep | `Remove | `Rewrite_with of Instrumentation.t ]

val eval : t -> condition:Dune.Instrumentation.Predicate.t Blang.t -> Dunolint.Trilang.t
val enforce : t -> condition:Dune.Instrumentation.Predicate.t Blang.t -> unit
