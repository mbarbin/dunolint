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

open! Import
include String_container_key

module Element = struct
  module Prefix = struct
    type t =
      | Ppx
      | Other

    let compare = (Stdlib.compare : t -> t -> int)
  end

  type t =
    { prefix : Prefix.t
    ; name : string
    }

  let compare a { prefix; name } =
    match Prefix.compare a.prefix prefix with
    | 0 -> compare_string a.name name
    | n -> n
  ;;
end

module Elements = struct
  type t = Element.t list

  let compare (a : t) b = compare_list Element.compare a b

  let of_name pp =
    let ts = String.split pp ~on:'.' in
    List.map ts ~f:(fun name ->
      let prefix =
        if String.is_prefix name ~prefix:"ppx_" then Element.Prefix.Ppx else Other
      in
      { Element.prefix; name })
    |> List.sort ~compare:Element.compare
  ;;
end

let compare pp1 pp2 =
  if String.equal pp1 pp2
  then 0
  else Elements.compare (Elements.of_name pp1) (Elements.of_name pp2)
;;

let invariant t =
  (not (String.is_empty t))
  && String.for_all t ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '_' || Char.equal c '-' || Char.equal c '.')
;;

include Validated_string.Make (struct
    let module_name = "Pp_name"
    let invariant = invariant
  end)
