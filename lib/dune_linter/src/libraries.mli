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

(** The ["libraries"] field indicates the dependencies for the stanza. It is
    used in stanza such as [library], [executable], etc. *)

type t

val create : libraries:Dune.Library.Name.t list -> t

(** At the moment there is not predicate nor enforceable conditions on
    libraries. They are automatically sorted. We may change this in the
    future, perhaps make the sorting optional, etc. TBD. *)
include Dunolinter.Stanza_linter.S with type t := t and type predicate := Nothing.t

(** {1 Getters} *)

module Entry : sig
  (** An entry in the [libraries] field. These are usually atoms referring to
      library names, but occasionally these can be more complex constructs. *)
  type t [@@deriving sexp_of]

  val library : Dune.Library.Name.t -> t
end

val is_empty : t -> bool
val entries : t -> Entry.t list
val mem : t -> library:Dune.Library.Name.t -> bool

(** {1 Setters} *)

val dedup_and_sort : t -> unit
val add_libraries : t -> libraries:Dune.Library.Name.t list -> unit
val add_entries : t -> entries:Entry.t list -> unit
