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

module Backend = struct
  module Name = struct
    include Container_key.String_impl

    let invariant t =
      (not (String.is_empty t))
      && String.for_all t ~f:(fun c ->
        Char.is_alphanum c || Char.equal c '_' || Char.equal c '.')
    ;;

    include Validated_string.Make (struct
        let module_name = "Dunolint.Instrumentation.Backend.Name"
        let invariant = invariant
      end)
  end

  module Flag = struct
    type t = string

    let equal = equal_string
    let t_of_sexp = String.t_of_sexp
    let sexp_of_t = String.sexp_of_t
  end

  type t =
    { name : Name.t
    ; flags : Flag.t list
    }

  let create ~name ~flags = { name; flags }
  let name t = t.name
  let flags t = t.flags
  let v ?(flags = []) name = { name = Name.v name; flags }

  let equal t1 ({ name; flags } as t2) =
    if Stdlib.( == ) t1 t2
    then true
    else Name.equal t1.name name && equal_list Flag.equal t1.flags flags
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    match sexp with
    | Atom _ -> { name = Name.t_of_sexp sexp; flags = [] }
    | List ((Atom _ as name_sexp) :: flag_sexps) ->
      { name = Name.t_of_sexp name_sexp; flags = List.map flag_sexps ~f:Flag.t_of_sexp }
    | List [] | List (List _ :: _) -> Sexplib0.Sexp_conv_error.no_variant_match ()
  ;;

  let sexp_of_t t : Sexp.t =
    match t.flags with
    | [] -> Name.sexp_of_t t.name
    | flags -> List (Name.sexp_of_t t.name :: List.map flags ~f:Flag.sexp_of_t)
  ;;
end

module Predicate = struct
  let error_source = "instrumentation.t"

  type t = [ `backend of Backend.t ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `backend va, `backend vb -> Backend.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "backend"
      ; conv =
          Variadic
            (fun ~context:_ ~fields ->
              match fields with
              | (Atom _ as name_sexp) :: flag_sexps ->
                `backend
                  { Backend.name = Backend.Name.t_of_sexp name_sexp
                  ; flags = List.map flag_sexps ~f:Backend.Flag.t_of_sexp
                  }
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source
                  "backend"
                  (Sexp.List []))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `backend { name; flags } ->
      List
        (Atom "backend"
         :: Backend.Name.sexp_of_t name
         :: List.map flags ~f:Backend.Flag.sexp_of_t)
  ;;
end
