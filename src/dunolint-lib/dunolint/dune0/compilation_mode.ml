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

  let error_source = "compilation_mode.t"

  type t =
    [ `byte
    | `native
    | `best
    | `melange
    ]

  let all = ([ `byte; `native; `best; `melange ] : t list)

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "byte"; conv = Nullary `byte }
    ; { atom = "native"; conv = Nullary `native }
    ; { atom = "best"; conv = Nullary `best }
    ; { atom = "melange"; conv = Nullary `melange }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    Atom
      (match t with
       | `byte -> "byte"
       | `native -> "native"
       | `best -> "best"
       | `melange -> "melange")
  ;;
end

include T

let to_comparable_int = function
  | `byte -> 0
  | `native -> 1
  | `best -> 2
  | `melange -> 3
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
