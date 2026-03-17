(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Dune_lang_version = Dune_lang_version
module Generate_opam_files = Generate_opam_files
module Implicit_transitive_deps = Implicit_transitive_deps
module Name = Name

type t

include Dunolinter.S with type t := t

type Stanza.t +=
  | Dune_lang_version of Dune_lang_version.t
  | Generate_opam_files of Generate_opam_files.t
  | Implicit_transitive_deps of Implicit_transitive_deps.t
  | Name of Name.t
