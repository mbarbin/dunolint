(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
