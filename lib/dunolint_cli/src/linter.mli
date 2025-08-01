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

val maybe_autoformat_file : previous_contents:string -> new_contents:string -> string

val lint_stanza
  :  rules:(Dunolint.Predicate.t, Dunolint.Predicate.t Blang.t) Dunolint.Rule.t list
  -> stanza:'a Dunolinter.Stanza.t
  -> return:unit With_return.return
  -> unit

module Visitor_decision : sig
  type t =
    | Continue
    | Skip_subtree
end

val visit_directory
  :  dunolint_engine:Dunolint_engine.t
  -> config:Dunolint.Config.t
  -> parent_dir:Relative_path.t
  -> files:string list
  -> Dunolint_engine.Visitor_decision.t
