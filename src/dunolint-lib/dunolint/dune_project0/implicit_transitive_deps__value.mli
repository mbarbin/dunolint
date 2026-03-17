(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Values for the implicit_transitive_deps stanza in dune-project files. *)

type t =
  [ `True
  | `False
  | `False_if_hidden_includes_supported
  ]

val all : t list

include Container_key.S with type t := t
