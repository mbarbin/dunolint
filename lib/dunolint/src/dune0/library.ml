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

module Modes = Library__modes
module Name = Library__name
module Public_name = Library__public_name

module Predicate = struct
  type t =
    [ `has_field of
        [ `instrumentation | `lint | `modes | `name | `preprocess | `public_name ]
    | `instrumentation of Instrumentation.Predicate.t Blang.t
    | `lint of Lint.Predicate.t Blang.t
    | `modes of Modes.Predicate.t Blang.t
    | `name of Name.Predicate.t Blang.t
    | `preprocess of Preprocess.Predicate.t Blang.t
    | `public_name of Public_name.Predicate.t Blang.t
    ]
  [@@deriving compare, equal, sexp]
end
