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

(** Helpers for dunolint tests.

    This module defines utils used by the tests of dunolint. It is not meant to
    be exported outside of the dunolint project, and thus is simply named
    "Test_helpers" rather than have a dunolint part in its name. *)

(** {1 Parsing Fields} *)

val read_sexp_field : path:Fpath.t -> string -> Sexps_rewriter.t * Sexp.t

val parse
  :  (module Dunolinter.Sexp_handler.S with type t = 'a)
  -> path:Fpath.t
  -> string
  -> (Sexps_rewriter.t * Sexp.t) * 'a

(** {1 Trilang} *)

val is_true : Dunolint.Trilang.t -> unit
val is_false : Dunolint.Trilang.t -> unit
val is_undefined : Dunolint.Trilang.t -> unit
