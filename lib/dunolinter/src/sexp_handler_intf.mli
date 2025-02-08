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

module type S = sig
  (** [t] is type OCaml representation of what the field encodes. It does not
      need to encapsulate the complete information that code be represented by
      dune, only that part that dunolint wants to lint. *)
  type t [@@deriving sexp_of]

  val field_name : string

  (** If the stanza already exists, parse the value currently present.
      The [sexps_rewriter] is given in order to find locations of the supplied
      sexp to supply to the error messages. Errors are raised if the arguments
      do not have the expected shape for that field. *)
  val read : sexps_rewriter:Sexps_rewriter.t -> field:Sexp.t -> t

  (** Write as a new field. This is used to create the field the first time it
      is introduced in a stanza, such as when a new dune file is created by
      dunolint. *)
  val write : t -> Sexp.t

  (** When the field is already present, it may be necessary to merge the
      existing values with the expected one. In this case we do not write the
      field from scratch, instead we refactor it using a rewrite. The [field]
      sexp is expected to be taken from the [sexps_rewriter] and must be the
      original sexp that represent the entire field.

      If the stanza already exists, all we do is some linting, that is we update
      it if needed, and perhaps generating some errors or warnings along the
      way. To be called with the sexps that follow the "(library)" atom, which
      are labeled "fields".

      This has the effect of side-effecting the [File_rewriter] that is
      contained by the [sexps_rewriter] parameter. For this call to be useful, it
      is assumed that the caller is going to output the resulting rewrite, and do
      something with it. The [field]
      sexp is expected to be taken from the [sexps_rewriter] and must be the
      original sexp that represent the entire field. *)
  val rewrite : t -> sexps_rewriter:Sexps_rewriter.t -> field:Sexp.t -> unit
end
