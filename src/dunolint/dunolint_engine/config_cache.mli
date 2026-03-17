(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Cache for loaded configs to avoid re-parsing the same file multiple times. *)

module Load_result : sig
  type t =
    | Absent
    | Present of Dunolint.Config.t
    | Error of Err.t
end

type t

val create : unit -> t
val load_config_in_dir : t -> dir:Relative_path.t -> Load_result.t
