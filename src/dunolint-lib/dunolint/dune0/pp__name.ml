(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
