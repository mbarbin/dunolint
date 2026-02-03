(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

module Compilation_mode = Dune0.Compilation_mode
module Executable = Dune0.Executable
module Include_subdirs = Dunolint.Dune.Include_subdirs
module Instrumentation = Dune0.Instrumentation
module Libraries = Dunolint.Dune.Libraries
module Library = Dune0.Library
module Lint = Dunolint.Dune.Lint
module Package = Dune0.Package
module Pp = Dune0.Pp
module Pps = Dunolint.Dune.Pps
module Predicate = Dunolint.Dune.Predicate
module Preprocess = Dunolint.Dune.Preprocess
module Stanza = Dunolint.Dune.Stanza
