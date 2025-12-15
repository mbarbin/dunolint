(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
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
