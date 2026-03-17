(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  [ `dune_lang_version of Dune_lang_version.Predicate.t Blang.t
  | `generate_opam_files of Generate_opam_files.Predicate.t Blang.t
  | `implicit_transitive_deps of Implicit_transitive_deps.Predicate.t Blang.t
  | `name of Name.Predicate.t Blang.t
  ]

val equal : t -> t -> bool
val sexp_of_t : t -> Sexplib0.Sexp.t
val t_of_sexp : Sexplib0.Sexp.t -> t
