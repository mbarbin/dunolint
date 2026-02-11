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
      (** Directory where this config was found relative to the workspace
          root. *)
    }
end

(** Add a discovered config at the specified location. *)
val add_config : t -> config:Dunolint.Config.t -> location:Relative_path.t -> t

(** Get the list of discovered configs with their locations. Returns configs in
    rule processing order: from least specific (root) to most specific (closest
    to current location), so that deeper configs can override rules from
    shallower configs. *)
val configs : t -> Config_with_location.t list

(** {1 Enclosing project metadata}

    During linting we aggregate ["dune-project"] files found in parent
    directories and retain some information used during linting.

    An example of information used is the dune lang version in use in the
    project, which informs the [dune format-dune-file --dune-version _] command
    used to perform the formatting of dune files.

    Dunolint supports performing linting operations when the enclosing
    dune-project file is invalid. In particular this allows more flexibility
    during development workflows such as linting-on-save.

    To achieve this, the information retained from a dune-project file is a
    result, and the code using this interface must handle the error case when
    querying the enclosing dune project context. *)

module Enclosing_result : sig
  type 'a t = ('a, Dune_project_context.Invalid_dune_project.t) Result.t
end

(** Add a discovered dune-project at the specified location. *)
val add_dune_project_context
  :  t
  -> dune_project_context:Dune_project_context.t Enclosing_result.t
  -> location:Relative_path.t
  -> t

(** If known to be within the scope of an enclosing dune-project file, returns
    the context we saved for it. This is used in particular to know which dune
    version to use when formatting dune files. This returns [None] when no dune
    project file was found in the enclosing context. *)
val enclosing_dune_project_context : t -> Dune_project_context.t Enclosing_result.t option

(** A convenient wrapper for {!enclosing_dune_project_context} which returns the
    lang version. If the ["dune-project"] is valid the dune lang version must
    have been found as its first line, otherwise we return [None] and rely on
    dune to report the missing stanza. *)
val enclosing_dune_lang_version
  :  t
  -> Dune_project.Dune_lang_version.t option Enclosing_result.t option
