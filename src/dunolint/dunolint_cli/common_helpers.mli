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

(** For use in the rest of the files in this directory. *)

val sexpable_param : (module Sexpable.S with type t = 'a) -> 'a Command.Param.t

(** Restrict the scope of a command to a subdirectory only. "Below this path".
    Accepts both relative and absolute paths. *)
val below : doc:string -> Fpath.t option Command.Arg.t

(** A list of defaults directories to skip. *)
val skip_subtrees : globs:string list -> Dunolint.Glob.t list

(** Create a default config with only skip_paths for common directories. *)
val default_skip_paths_config : unit -> Dunolint.Config.t

(** Create a config containing only the given enforce rules, or None if the list
    is empty. *)
val enforce_rules_config : rules:Dunolint.Config.Rule.t list -> Dunolint.Config.t option

(** Override the workspace root - same as with dune. *)
val root : Absolute_path.t option Command.Arg.t

(** When supplying path arguments that are aimed to designate paths in
    workspace, we need to resolve them according to where the [workspace_root]
    is in relation to the cwd. We interpret relative paths as relative to the
    [cwd] from which the program started. We use this helper for example to
    resolve arguments such as [--below _] or [--config _]. *)
val relativize
  :  workspace_root:Workspace_root.t
  -> cwd:Absolute_path.t
  -> path:Fpath.t
  -> Relative_path.t
