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

  let error_source = "linted_file_kind.t"

  type t =
    [ `dune
    | `dune_project
    | `dune_workspace
    | `dunolint
    ]

  let all = ([ `dune; `dune_project; `dune_workspace; `dunolint ] : t list)

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "dune"; conv = Nullary `dune }
    ; { atom = "dune_project"; conv = Nullary `dune_project }
    ; { atom = "dune_workspace"; conv = Nullary `dune_workspace }
    ; { atom = "dunolint"; conv = Nullary `dunolint }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `dune -> Atom "dune"
    | `dune_project -> Atom "dune_project"
    | `dune_workspace -> Atom "dune_workspace"
    | `dunolint -> Atom "dunolint"
  ;;
end

include T

let to_string = function
  | `dune -> "dune"
  | `dune_project -> "dune-project"
  | `dune_workspace -> "dune-workspace"
  | `dunolint -> "dunolint"
;;

let of_string = function
  | "dune" -> Ok `dune
  | "dune-project" -> Ok `dune_project
  | "dune-workspace" -> Ok `dune_workspace
  | "dunolint" -> Ok `dunolint
  | str -> Error (`Msg (Printf.sprintf "Invalid linted file kind: %S" str))
;;

let to_comparable_int = function
  | `dune -> 0
  | `dune_project -> 1
  | `dune_workspace -> 2
  | `dunolint -> 3
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
