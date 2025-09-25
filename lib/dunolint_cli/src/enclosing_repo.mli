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

(** This application works from within a Git or Mercurial repository.

    This module specifies which functionality the rest of the application relies
    on, by defining the specific set of traits that are required. We make use of
    the [volgo] library for this. *)

module Vcs_kind : sig
  (** The kind of vcs supported by the cli. *)
  type t =
    [ `Git
    | `Hg
    ]
  [@@deriving sexp_of]
end

(** The specific list of traits that must be implemented by a vcs backend in
    order for it to be used by the cli. *)
type vcs = < Vcs.Trait.file_system ; Vcs.Trait.ls_files > Vcs.t

(** A type to represent the vcs found when walking up from within a directory
    located inside a repo. *)
type t =
  { vcs_kind : Vcs_kind.t
  ; repo_root : Vcs.Repo_root.t
  ; vcs : vcs
  }
