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

module File_kind = File_kind
module Running_mode = Running_mode
module Config_cache = Config_cache
module Context = Context

type t

(** {1 Context building} *)

(** Build a context for a given file path by auto-discovering and loading
    configs from parent directories, and applying the engine's root configs.

    This is useful for tools that need to lint individual files without
    performing a full directory traversal via [visit].

    [path] is the relative path to the file for which to build the context.

    The function returns a context containing:
    - The engine's root configs (specified at engine creation), which serve as
      base defaults
    - All configs auto-discovered from the workspace root down to and including
      the directory containing the file, which take precedence over root
      configs *)
val build_context : t -> path:Relative_path.t -> Context.t

(** {1 Execution control}*)

module Visitor_decision : sig
  (** While we visit the repository, we may decide what to do at each step of
      the iteration. *)

  type t =
    | Break (** Stops the current execution of [visit]. *)
    | Continue
    (** Recurse and visit the children of the current sexp if any, or continue
        with the next directory in the queue. *)
    | Skip_subtree
    (** Do not drill in, skip the current directory and continue with the next
        directory in the queue. If the current directory does not have
        subdirectory, this is equivalent to [Continue], which should be
        preferred by default. *)
end

(** Visit the directory tree.

    [parent_dir] contains the complete relative path to the [cwd] from which the
    command originated, which should be assumed is the dune workspace root.

    [subdirectories] and [files] are the base names of the contents of the
    parent directory currently being visited.

    [autoload_config] controls whether to automatically discover and load config
    files during traversal. Defaults to [true].

    [root_configs] is a list of base default configs applied with lowest
    precedence, overridden by any auto-discovered configs. Defaults to the empty
    list.

    [context] contains the accumulated configuration context for the current
    directory being visited.

    If you want to visit only a subtree of the workspace, you may provide
    [below], which must be a relative path to a subdirectory of the [cwd]
    provided for creating [t]. *)
val visit
  :  ?autoload_config:bool (** defaults to [true] *)
  -> ?below:Relative_path.t
  -> t
  -> f:
       (context:Context.t
        -> parent_dir:Relative_path.t
        -> subdirectories:string list
        -> files:string list
        -> Visitor_decision.t)
  -> unit

(** {1 Lint} *)

val lint_dune_file
  :  ?with_linter:(Dune_linter.t -> unit)
  -> t
  -> path:Relative_path.t
  -> f:(Dune_linter.Stanza.t Dunolinter.Stanza.t -> unit)
  -> unit

val lint_dune_project_file
  :  ?with_linter:(Dune_project_linter.t -> unit)
  -> t
  -> path:Relative_path.t
  -> f:(Dune_project_linter.Stanza.t Dunolinter.Stanza.t -> unit)
  -> unit

val lint_file
  :  ?autoformat_file:(new_contents:string -> string)
  -> ?create_file:(unit -> string)
  -> ?rewrite_file:(previous_contents:string -> string)
  -> t
  -> path:Relative_path.t
  -> unit

(** Spawn a [dune format-dune-file] on the new linted contents before
    materializing into a file. Exposed if you need to write your own linters on
    files that are supported by the formatter shipped with dune. *)
val format_dune_file : new_contents:string -> string

(** This calls [f] once, registers all requests enqueued during the execution of
    [f], and then depending on the running mode, either do a dry-run, or
    actually perform the desired transformations.

    The intended use is for [f] to contain one or several calls to a function
    that uses [t] to perform some dunolint linting, such as [visit],
    [lint_dune_file], etc.

    This is a convenience wrapper around [create], calling [f], and
    [materialize]. The [root_configs] and [running_mode] parameters are
    forwarded to [create].

    In addition to enqueuing debug messages and errors, this function outputs
    messages regarding I/O actions executed during linting. These messages are
    produced onto [stdout]. *)
val run
  :  ?root_configs:Dunolint.Config.t list
  -> running_mode:Running_mode.t
  -> (t -> 'a)
  -> 'a

(** {1 Step by step API} *)

(** Create a new linting engine.

    The [root_configs] parameter allows specifying configs that will be included
    in the context passed to the visit callback. These configs are treated as if
    they were located at the workspace root.

    The [running_mode] determines whether changes are applied (Force_yes),
    previewed (Dry_run), interactively confirmed (Interactive), or checked
    without modification (Check). *)
val create
  :  ?root_configs:Dunolint.Config.t list (** defaults to [[]] *)
  -> running_mode:Running_mode.t
  -> unit
  -> t

(** Apply all the changes that have been saved into [t] to the file system, or
    merely print them if we're in dry-run mode. *)
val materialize : t -> unit

module Private : sig
  val mkdirs : Relative_path.t -> unit

  (** Path operations for workspace-relative paths with escaping prevention.

      This module is exported for testing purposes. See {!Path_in_workspace} for
      documentation. *)
  module Path_in_workspace = Path_in_workspace
end
