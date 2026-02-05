(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
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
