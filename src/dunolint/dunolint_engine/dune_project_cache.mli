(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Cache for loaded dune-project to avoid re-parsing the same file multiple times.

    This is used for saving information from dune-project files used as
    enclosing context when linting other files. Linting of [dune-project] files
    themselves goes via another execution path. *)

module Load_result : sig
  type t =
    | Absent
    | Present of
        (Dune_project_context.t, Dune_project_context.Invalid_dune_project.t) Result.t
end

type t

val create : unit -> t
val load_dune_project_in_dir : t -> dir:Relative_path.t -> Load_result.t
