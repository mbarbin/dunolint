(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A directory, or a glob, of a stanza such as [(dirs ...)] or [(data_only_dirs ...)]. *)

type t =
  { name : string (** Its value. *)
  ; sexp : Sexp.t
    (** The atom of the source, kept for its identity in the [sexps_rewriter]. *)
  }

(** Prints the name only. *)
val sexp_of_t : t -> Sexp.t

(** Read an atom of the source. *)
val read : sexps_rewriter:Sexps_rewriter.t -> Sexp.t -> t
