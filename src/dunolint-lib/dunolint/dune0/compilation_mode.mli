(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A variant used by dune in the configuration of the compilation and linking
    modes of build artifacts such as libraries and executables. *)

type t =
  [ `byte
  | `native
  | `best
  | `melange
  ]

val all : t list

include Container_key.S with type t := t
