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

(** The ["preprocess"] field indicates the preprocessor to be used. It is used
    in stanza such as [library], [executable], etc. *)

type t

val create : ?pps:Pps.t -> unit -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate := Dune.Preprocess.Predicate.t

(** {1 Getters} *)

module State : sig
  type t =
    | No_preprocessing
    | Pps of Pps.t
    | Unhandled of Sexp.t
  [@@deriving sexp_of]
end

val state : t -> State.t

(** {1 Setters} *)

val set_state : t -> state:State.t -> unit
