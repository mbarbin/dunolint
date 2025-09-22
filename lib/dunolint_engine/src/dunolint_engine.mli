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

type t

(** {1 Execution control}*)

module Visitor_decision : sig
  (** While we visit the repository, we may decide what to do at each step of the
      iteration. *)

  type t =
    | Break (** Stops the current execution of [visit]. *)
    | Continue
    (** Recurse and visit the children of the current sexp if any, or
        continue with the next directory in the queue. *)
    | Skip_subtree
    (** Do not drill in, skip the current directory and continue
        with the next directory in the queue. If the current directory
        does not have subdirectory, this is equivalent to [Continue],
        which should be preferred by default. *)
end

(** [parent_dir] contains the complete relative path to the [cwd] from which the
    command originated, which should be assumed is the root of the repository.

    [subdirectories] and [files] are the base names of the contents of the
    parent directory currently being visited.

    If you want to visit only a subtree of the repository, you may provide
    [below], which must be a relative path to a subdirectory of the [cwd]
    provided for creating [t]. *)
val visit
  :  ?below:Relative_path.t
  -> t
  -> f:
       (parent_dir:Relative_path.t
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
    materializing into a file. Exposed if you need to write your own linters
    on files that are supported by the formatter shipped with dune. *)
val format_dune_file : new_contents:string -> string

(** This calls [f] once, registers all requests enqueued during the execution of
    [f], and then depending on the running mode, either do a dry-run, or
    actually perform the desired transformations.

    The intended use is for [f] to contain one or several calls to a function
    that uses [t] to perform some dunolint linting, such as [visit],
    [lint_dune_file], etc.

    The [process_mgr] argument is used in order to spawn [dune format-dune-file]
    processes to reformat dune files after they have been linted.

    In addition to enqueuing debug messages and errors, this function outputs
    messages regarding I/O actions executed during linting. These messages are
    produced onto [stdout]. *)
val run : running_mode:Running_mode.t -> (t -> 'a) -> 'a

(** {1 Step by step API} *)

val create : running_mode:Running_mode.t -> unit -> t

(** Apply all the changes that have been saved into [t] to the file system, or
    merely print them if we're in dry-run mode. *)
val materialize : t -> unit

module Private : sig
  val mkdirs : Relative_path.t -> unit
end
