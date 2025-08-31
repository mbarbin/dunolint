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
  type t =
    [ `True
    | `False
    | `False_if_hidden_includes_supported
    ]
  [@@deriving enumerate]

  let of_string = function
    | "true" -> Some `True
    | "false" -> Some `False
    | "false-if-hidden-includes-supported" -> Some `False_if_hidden_includes_supported
    | _ -> None
  ;;

  let to_string = function
    | `True -> "true"
    | `False -> "false"
    | `False_if_hidden_includes_supported -> "false-if-hidden-includes-supported"
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let t_of_sexp = function
    | Sexp.Atom s ->
      (match of_string s with
       | Some v -> v
       | None -> failwith ("Invalid implicit_transitive_deps value: " ^ s))
    | _ -> failwith "Expected atom for implicit_transitive_deps value"
  ;;

  let compare a b =
    let to_int = function
      | `True -> 0
      | `False -> 1
      | `False_if_hidden_includes_supported -> 2
    in
    Int.compare (to_int a) (to_int b)
  ;;
end

include T
include Comparable.Make (T)

let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
