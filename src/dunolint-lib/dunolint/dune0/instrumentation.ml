(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
    if phys_equal t1 t2
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

  type t =
    [ `backend of Backend.t
    | `present of Backend.Name.t Nonempty_list.t
    | `absent of Backend.Name.t Nonempty_list.t
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `backend va, `backend vb -> Backend.equal va vb
      | `present (a :: va), `present (b :: vb) | `absent (a :: va), `absent (b :: vb) ->
        equal_list Backend.Name.equal (a :: va) (b :: vb)
      | (`backend _ | `present _ | `absent _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    let names (f : Backend.Name.t Nonempty_list.t -> t) =
      Sexp_helpers.Variant_spec.Nonempty
        (fun ~context:_ ~fields:(hd :: tl) ->
          f (Backend.Name.t_of_sexp hd :: List.map tl ~f:Backend.Name.t_of_sexp))
    in
    [ { atom = "backend"
      ; conv =
          Nonempty
            (fun ~context:_ ~fields:(name_sexp :: flag_sexps) ->
              match name_sexp with
              | Atom _ ->
                `backend
                  { Backend.name = Backend.Name.t_of_sexp name_sexp
                  ; flags = List.map flag_sexps ~f:Backend.Flag.t_of_sexp
                  }
              | List _ ->
                Sexp_helpers.raise
                  name_sexp
                  ~message:"The construct [backend] expects the name of a backend first."
                  ~suggestion:"Replace by: (backend NAME FLAG...)")
      }
    ; { atom = "present"; conv = names (fun v -> `present v) }
    ; { atom = "absent"; conv = names (fun v -> `absent v) }
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
    | `present (hd :: tl) ->
      List (Atom "present" :: List.map (hd :: tl) ~f:Backend.Name.sexp_of_t)
    | `absent (hd :: tl) ->
      List (Atom "absent" :: List.map (hd :: tl) ~f:Backend.Name.sexp_of_t)
  ;;
end
