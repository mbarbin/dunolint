(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A utility module to help with the loading and parsing of dunolint config
    files. *)

(** A helper for loading the config with some effort regarding producing located
    error messages when able. *)
val load_config_exn : filename:string -> Dunolint.Config.t
