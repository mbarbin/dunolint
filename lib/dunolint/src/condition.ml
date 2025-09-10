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

module T = struct
  [@@@coverage off]

  type t = Predicate.t Blang.t [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__001_ -> fun b__002_ -> Blang.compare Predicate.compare a__001_ b__002_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__005_ -> fun b__006_ -> Blang.equal Predicate.equal a__005_ b__006_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__010_ -> Blang.t_of_sexp Predicate.t_of_sexp x__010_ : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__011_ -> Blang.sexp_of_t Predicate.sexp_of_t x__011_ : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end

include T
