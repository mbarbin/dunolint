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

module Predicate = struct
  module Param = struct
    let error_source = "pps.param.t"

    type t =
      [ `any
      | `none
      | `some
      | `equals of string
      ]

    let equal (a : t) (b : t) =
      match a, b with
      | `any, `any -> true
      | `none, `none -> true
      | `some, `some -> true
      | `equals va, `equals vb -> phys_equal a b || equal_string va vb
      | (`any | `none | `some | `equals _), _ -> false
    ;;

    let variant_spec : t Sexp_helpers.Variant_spec.t =
      [ { atom = "any"; conv = Nullary `any }
      ; { atom = "none"; conv = Nullary `none }
      ; { atom = "some"; conv = Nullary `some }
      ; { atom = "equals"; conv = Unary (fun sexp -> `equals (string_of_sexp sexp)) }
      ]
    ;;

    let t_of_sexp (sexp : Sexp.t) : t =
      Sexp_helpers.parse_variant variant_spec ~error_source sexp
    ;;

    let sexp_of_t (t : t) : Sexp.t =
      match t with
      | `any -> Atom "any"
      | `none -> Atom "none"
      | `some -> Atom "some"
      | `equals v -> List [ Atom "equals"; sexp_of_string v ]
    ;;
  end

  module Flag = struct
    module Applies_to = struct
      let error_source = "pps.flag.applies_to.t"

      type t =
        [ `any
        | `driver
        | `pp of Pp.Name.t
        ]

      let equal (a : t) (b : t) =
        match a, b with
        | `any, `any -> true
        | `driver, `driver -> true
        | `pp va, `pp vb -> phys_equal a b || Pp.Name.equal va vb
        | (`any | `driver | `pp _), _ -> false
      ;;

      let variant_spec : t Sexp_helpers.Variant_spec.t =
        [ { atom = "any"; conv = Nullary `any }
        ; { atom = "driver"; conv = Nullary `driver }
        ; { atom = "pp"; conv = Unary (fun sexp -> `pp (Pp.Name.t_of_sexp sexp)) }
        ]
      ;;

      let t_of_sexp (sexp : Sexp.t) : t =
        Sexp_helpers.parse_variant variant_spec ~error_source sexp
      ;;

      let sexp_of_t (t : t) : Sexp.t =
        match t with
        | `any -> Atom "any"
        | `driver -> Atom "driver"
        | `pp v -> List [ Atom "pp"; Pp.Name.sexp_of_t v ]
      ;;
    end

    let error_source = "pps.flag.t"

    type t =
      { name : string
      ; param : Param.t
      ; applies_to : Applies_to.t
      }

    let equal (a : t) (b : t) =
      if phys_equal a b
      then true
      else (
        let { name; param; applies_to } = b in
        equal_string a.name name
        && Param.equal a.param param
        && Applies_to.equal a.applies_to applies_to)
    ;;

    let t_of_sexp sexp =
      Sexplib0.Sexp_conv_record.record_of_sexp
        ~caller:error_source
        ~fields:
          (Field
             { name = "name"
             ; kind = Required
             ; conv = string_of_sexp
             ; rest =
                 Field
                   { name = "param"
                   ; kind = Required
                   ; conv = Param.t_of_sexp
                   ; rest =
                       Field
                         { name = "applies_to"
                         ; kind = Required
                         ; conv = Applies_to.t_of_sexp
                         ; rest = Empty
                         }
                   }
             })
        ~index_of_field:(function
          | "name" -> 0
          | "param" -> 1
          | "applies_to" -> 2
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (name, (param, (applies_to, ()))) ->
          ({ name; param; applies_to } : t))
        sexp
    ;;

    let sexp_of_t { name; param; applies_to } : Sexp.t =
      List
        [ List [ Atom "name"; sexp_of_string name ]
        ; List [ Atom "param"; Param.sexp_of_t param ]
        ; List [ Atom "applies_to"; Applies_to.sexp_of_t applies_to ]
        ]
    ;;
  end

  module Pp_with_flag = struct
    let error_source = "pps.pp_with_flag.t"

    type t =
      { pp : Pp.Name.t
      ; flag : string
      ; param : Param.t
      }

    let equal (a : t) (b : t) =
      if phys_equal a b
      then true
      else (
        let { pp; flag; param } = b in
        Pp.Name.equal a.pp pp && equal_string a.flag flag && Param.equal a.param param)
    ;;

    let t_of_sexp sexp =
      Sexplib0.Sexp_conv_record.record_of_sexp
        ~caller:error_source
        ~fields:
          (Field
             { name = "pp"
             ; kind = Required
             ; conv = Pp.Name.t_of_sexp
             ; rest =
                 Field
                   { name = "flag"
                   ; kind = Required
                   ; conv = string_of_sexp
                   ; rest =
                       Field
                         { name = "param"
                         ; kind = Required
                         ; conv = Param.t_of_sexp
                         ; rest = Empty
                         }
                   }
             })
        ~index_of_field:(function
          | "pp" -> 0
          | "flag" -> 1
          | "param" -> 2
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (pp, (flag, (param, ()))) -> ({ pp; flag; param } : t))
        sexp
    ;;

    let sexp_of_t { pp; flag; param } : Sexp.t =
      List
        [ List [ Atom "pp"; Pp.Name.sexp_of_t pp ]
        ; List [ Atom "flag"; sexp_of_string flag ]
        ; List [ Atom "param"; Param.sexp_of_t param ]
        ]
    ;;
  end

  let error_source = "pps.t"

  type t =
    [ `pp of Pp.Name.t
    | `flag of Flag.t
    | `pp_with_flag of Pp_with_flag.t
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `pp va, `pp vb -> Pp.Name.equal va vb
      | `flag va, `flag vb -> Flag.equal va vb
      | `pp_with_flag va, `pp_with_flag vb -> Pp_with_flag.equal va vb
      | (`pp _ | `flag _ | `pp_with_flag _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "pp"; conv = Unary (fun sexp -> `pp (Pp.Name.t_of_sexp sexp)) }
    ; { atom = "flag"
      ; conv =
          Variadic
            (fun ~context ~fields ->
              `flag
                (Sexp_helpers.parse_inline_record
                   (module Flag)
                   ~error_source
                   ~context
                   ~tag:"flag"
                   ~fields))
      }
    ; { atom = "pp_with_flag"
      ; conv =
          Variadic
            (fun ~context ~fields ->
              `pp_with_flag
                (Sexp_helpers.parse_inline_record
                   (module Pp_with_flag)
                   ~error_source
                   ~context
                   ~tag:"pp_with_flag"
                   ~fields))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `pp v -> List [ Atom "pp"; Pp.Name.sexp_of_t v ]
    | `flag v ->
      let sexps =
        match Flag.sexp_of_t v with
        | List sexps -> sexps
        | Atom _ -> assert false
      in
      List (Atom "flag" :: sexps)
    | `pp_with_flag v ->
      let sexps =
        match Pp_with_flag.sexp_of_t v with
        | List sexps -> sexps
        | Atom _ -> assert false
      in
      List (Atom "pp_with_flag" :: sexps)
  ;;
end
