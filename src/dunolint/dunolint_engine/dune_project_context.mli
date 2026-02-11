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

(** Context associated with a [dune-project] file.

    When performing linting it may be useful to refer to context from the
    enclosing dune-project file. This type is used to hold the information we
    wish to access. *)

(** Each value of type [t] refers to context found in exactly one file. This
    type is non mutable. *)
type t

(** [create ~path ~original_contents] parses information needed from a
    ["dune-project"] file, and extracts from it an immutable context required
    for linting. *)
val create : path:Relative_path.t -> original_contents:string -> (t, Err.t) Result.t

(** {1 Getters} *)

(** A valid [dune-project] file will necessarily start with a valid lang dune
    stanza, however if the file is not valid, this may return [None]. *)
val dune_lang_version : t -> Dunolint.Dune_project.Dune_lang_version.t option

module Invalid_dune_project : sig
  (** Logging the actual error must be centralized and done once, so the calling
      code shall simply handle the fact that an error occurred. *)
  type t = private Invalid_dune_project

  val acknowledge : Err.t -> t
end
