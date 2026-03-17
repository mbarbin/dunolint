(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A utility module to help with the management of comments in sexps files. *)

(** [extended_range] computes the range for a library entry, that includes the
    original range for the entry, but where the [stop] offset of the range may be
    shifted to the right, until the end of the line, if this captures a comment
    placed on the same line as the value.

    For example:

    {v
     (libraries
        foo
        bar ;; a comment for bar on the same line
        baz)
    v}

    [extended_range foo] will be [foo]'s original range unchanged. And
    [extended_range bar] will include bar and its comment too. *)
val extended_range : original_contents:string -> range:Loc.Range.t -> Loc.Range.t

(** A convenient wrapper for [extended_range] that specializes to sexp arguments
    found when rewritting sexps with [Sexps_rewriter]. *)
val sexp_extended_range : sexps_rewriter:Sexps_rewriter.t -> arg:Sexp.t -> Loc.Range.t

(** A convenient wrapper that extracts and returns the substring matching the
    entire [extended_range] from the original contents. *)
val get_extended_source : original_contents:string -> range:Loc.Range.t -> string
