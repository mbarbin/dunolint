(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  | Dry_run
  | Check
  | Force_yes
  | Interactive
[@@deriving compare, equal, sexp_of]

val default : t
val arg : t Cmdlang.Command.Arg.t
