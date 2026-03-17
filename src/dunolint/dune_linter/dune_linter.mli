(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Executable = Executable
module Flags = Flags
module Include_subdirs = Include_subdirs
module Instrumentation = Instrumentation
module Library = Library
module Libraries = Libraries
module Lint = Lint
module Pps = Pps
module Preprocess = Preprocess

type t

include Dunolinter.S with type t := t

type Stanza.t +=
  | Include_subdirs of Include_subdirs.t
  | Library of Library.t
  | Executable of Executable.t
