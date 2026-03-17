(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  [ `executable of Executable.Predicate.t Blang.t
  | `has_field of [ `instrumentation | `lint | `name | `preprocess | `public_name ]
  | `include_subdirs of Include_subdirs.Predicate.t Blang.t
  | `instrumentation of Instrumentation.Predicate.t Blang.t
  | `libraries of Libraries.Predicate.t Blang.t
  | `library of Library.Predicate.t Blang.t
  | `lint of Lint.Predicate.t Blang.t
  | `preprocess of Preprocess.Predicate.t Blang.t
  | `stanza of Stanza.Predicate.t Blang.t
  ]

val equal : t -> t -> bool

include Sexpable.S with type t := t
