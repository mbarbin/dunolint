(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  [ `dune
  | `dune_project
  | `dune_workspace
  | `dunolint
  ]

val all : t list
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) Result.t

include Container_key.S with type t := t
