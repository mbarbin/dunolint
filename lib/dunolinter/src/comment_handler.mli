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

(** Tell whether two consecutive arguments are to be treated as belonging to
    different sections.

    The way dunolint does this, is to look whether two consecutive entries are
    separated by more than 1 line. In particular this covers the case where
    entries are separated by a comment in its own line, in which case dunolint
    will consider that the dependencies are in different sections.

    {v
     (libraries
       aa
       bb
       ;; this a comment
       cc
       zz)
    v}

    [are_in_different_section] must be called with two consecutive arguments,
    otherwise the returned value does not have any particular meaning. *)
val are_in_different_sections
  :  previous:Parsexp.Positions.range
  -> current:Parsexp.Positions.range
  -> bool
