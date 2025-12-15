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

(** Auto-discovery and accumulation of contextual information during tree
    traversal.

    This module provides a context that is built up as the linting engine
    traverses the directory tree, accumulating information that affects how
    linting is performed.

    The context currently holds a list of dunolint configurations. The order
    follows the principle of a functional stack: data is added at the head as it
    is discovered when going deeper in the directory structure, and the
    {!configs} function returns them in rule processing order (shallowest to
    deepest).

    {2 Planned evolution}

    The context is designed to support future enhancements:

    + {b Config autoloading}: Automatically discover and load dunolint config
      files from the workspace root and subdirectories during traversal, with
      location tracking to enable path-relative rule evaluation.
    + {b Additional dune context}: Include information from enclosing
      dune-project files and dune describe output to provide richer context
      for linting rules (e.g., project metadata, library dependencies). *)

type t

(** An empty context. *)
val empty : t

(** Configuration with its location in the directory tree. *)
module Config_with_location : sig
  type t =
    { config : Dunolint.Config.t
    ; location : Relative_path.t
      (** Directory where this config was found
          relative to the workspace root. *)
    }
end

(** Add a discovered config at the specified location. *)
val add_config : t -> config:Dunolint.Config.t -> location:Relative_path.t -> t

(** Get the list of discovered configs with their locations.
    Returns configs in rule processing order: from least specific (root) to
    most specific (closest to current location), so that deeper configs can
    override rules from shallower configs. *)
val configs : t -> Config_with_location.t list
