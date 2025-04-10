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

(** The ["pps"] field indicates the preprocessor to be used. It is used as a sub
    field of the [preprocess] and [lint] fields that are found in [library],
    [executable], etc. *)

type t

module Arg : sig
  type t =
    | Pp of Dune.Pp.Name.t
    | Flag of
        { name : string
        ; param : string option
        }
  [@@deriving equal, sexp_of]
end

val create : args:Arg.t list -> t

include
  Dunolinter.Stanza_linter.S with type t := t and type predicate := Dune.Pps.Predicate.t

(** {1 Utils} *)

(** A util to create a value from a list of atoms. The [loc] is used for error
    messages, [Loc.none] may be supplied if no loc is available. *)
val parse : loc:Loc.t -> string list -> t
