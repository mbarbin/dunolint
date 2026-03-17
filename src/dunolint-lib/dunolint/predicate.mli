(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  [ `path of Path.Predicate.t Blang.t
  | `dune of Dune.Predicate.t Blang.t
  | `dune_project of Dune_project.Predicate.t Blang.t
  | `dune_workspace of Dune_workspace.Predicate.t Blang.t
  | `dunolint of Dunolint0.Predicate.t Blang.t
  ]

val equal : t -> t -> bool

include Sexpable.S with type t := t
