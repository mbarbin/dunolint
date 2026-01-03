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

module Modes = Library__modes
module Name = Library__name
module Package = Library__package
module Public_name = Library__public_name

module Predicate = struct
  let error_source = "library.t"

  module Has_field = struct
    type t =
      [ `inline_tests
      | `instrumentation
      | `lint
      | `modes
      | `name
      | `package
      | `preprocess
      | `public_name
      ]

    let equal = (Stdlib.( = ) : t -> t -> bool)

    let variant_spec : t Sexp_helpers.Variant_spec.t =
      [ { atom = "inline_tests"; conv = Nullary `inline_tests }
      ; { atom = "instrumentation"; conv = Nullary `instrumentation }
      ; { atom = "lint"; conv = Nullary `lint }
      ; { atom = "modes"; conv = Nullary `modes }
      ; { atom = "name"; conv = Nullary `name }
      ; { atom = "package"; conv = Nullary `package }
      ; { atom = "preprocess"; conv = Nullary `preprocess }
      ; { atom = "public_name"; conv = Nullary `public_name }
      ]
    ;;

    let t_of_sexp (sexp : Sexp.t) : t =
      Sexp_helpers.parse_variant variant_spec ~error_source sexp
    ;;

    let sexp_of_t (t : t) : Sexp.t =
      Atom
        (match t with
         | `inline_tests -> "inline_tests"
         | `instrumentation -> "instrumentation"
         | `lint -> "lint"
         | `modes -> "modes"
         | `name -> "name"
         | `package -> "package"
         | `preprocess -> "preprocess"
         | `public_name -> "public_name")
    ;;
  end

  type t =
    [ `has_field of Has_field.t
    | `instrumentation of Instrumentation.Predicate.t Blang.t
    | `lint of Lint.Predicate.t Blang.t
    | `modes of Modes.Predicate.t Blang.t
    | `name of Name.Predicate.t Blang.t
    | `package of Package.Predicate.t Blang.t
    | `preprocess of Preprocess.Predicate.t Blang.t
    | `public_name of Public_name.Predicate.t Blang.t
    | `if_present of
        [ `package of Package.Predicate.t Blang.t
        | `public_name of Public_name.Predicate.t Blang.t
        ]
    ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `has_field va, `has_field vb -> Has_field.equal va vb
      | `instrumentation va, `instrumentation vb ->
        Blang.equal Instrumentation.Predicate.equal va vb
      | `lint va, `lint vb -> Blang.equal Lint.Predicate.equal va vb
      | `modes va, `modes vb -> Blang.equal Modes.Predicate.equal va vb
      | `name va, `name vb -> Blang.equal Name.Predicate.equal va vb
      | `package va, `package vb -> Blang.equal Package.Predicate.equal va vb
      | `preprocess va, `preprocess vb -> Blang.equal Preprocess.Predicate.equal va vb
      | `public_name va, `public_name vb -> Blang.equal Public_name.Predicate.equal va vb
      | `if_present p1, `if_present p2 ->
        (match p1, p2 with
         | `package va, `package vb -> Blang.equal Package.Predicate.equal va vb
         | `public_name va, `public_name vb ->
           Blang.equal Public_name.Predicate.equal va vb
         | (`package _ | `public_name _), _ -> false)
      | ( ( `has_field _
          | `instrumentation _
          | `lint _
          | `modes _
          | `name _
          | `package _
          | `preprocess _
          | `public_name _
          | `if_present _ )
        , _ ) -> false)
  ;;

  let rec variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "has_field"
      ; conv = Unary (fun sexp -> `has_field (Has_field.t_of_sexp sexp))
      }
    ; { atom = "instrumentation"
      ; conv =
          Unary
            (fun sexp ->
              `instrumentation (Blang.t_of_sexp Instrumentation.Predicate.t_of_sexp sexp))
      }
    ; { atom = "lint"
      ; conv = Unary (fun sexp -> `lint (Blang.t_of_sexp Lint.Predicate.t_of_sexp sexp))
      }
    ; { atom = "modes"
      ; conv = Unary (fun sexp -> `modes (Blang.t_of_sexp Modes.Predicate.t_of_sexp sexp))
      }
    ; { atom = "name"
      ; conv = Unary (fun sexp -> `name (Blang.t_of_sexp Name.Predicate.t_of_sexp sexp))
      }
    ; { atom = "package"
      ; conv =
          Unary (fun sexp -> `package (Blang.t_of_sexp Package.Predicate.t_of_sexp sexp))
      }
    ; { atom = "preprocess"
      ; conv =
          Unary
            (fun sexp ->
              `preprocess (Blang.t_of_sexp Preprocess.Predicate.t_of_sexp sexp))
      }
    ; { atom = "public_name"
      ; conv =
          Unary
            (fun sexp ->
              `public_name (Blang.t_of_sexp Public_name.Predicate.t_of_sexp sexp))
      }
    ; { atom = "if_present"
      ; conv = Unary (fun sexp -> `if_present (if_present_of_sexp sexp))
      }
    ]

  and if_present_variant_spec
    : [ `package of Package.Predicate.t Blang.t
      | `public_name of Public_name.Predicate.t Blang.t
      ]
        Sexp_helpers.Variant_spec.t
    =
    [ { atom = "package"
      ; conv =
          Unary (fun sexp -> `package (Blang.t_of_sexp Package.Predicate.t_of_sexp sexp))
      }
    ; { atom = "public_name"
      ; conv =
          Unary
            (fun sexp ->
              `public_name (Blang.t_of_sexp Public_name.Predicate.t_of_sexp sexp))
      }
    ]

  and if_present_of_sexp sexp =
    Sexp_helpers.parse_variant if_present_variant_spec ~error_source sexp
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `has_field v -> List [ Atom "has_field"; Has_field.sexp_of_t v ]
    | `instrumentation v ->
      List
        [ Atom "instrumentation"; Blang.sexp_of_t Instrumentation.Predicate.sexp_of_t v ]
    | `lint v -> List [ Atom "lint"; Blang.sexp_of_t Lint.Predicate.sexp_of_t v ]
    | `modes v -> List [ Atom "modes"; Blang.sexp_of_t Modes.Predicate.sexp_of_t v ]
    | `name v -> List [ Atom "name"; Blang.sexp_of_t Name.Predicate.sexp_of_t v ]
    | `package v -> List [ Atom "package"; Blang.sexp_of_t Package.Predicate.sexp_of_t v ]
    | `preprocess v ->
      List [ Atom "preprocess"; Blang.sexp_of_t Preprocess.Predicate.sexp_of_t v ]
    | `public_name v ->
      List [ Atom "public_name"; Blang.sexp_of_t Public_name.Predicate.sexp_of_t v ]
    | `if_present p ->
      List
        [ Atom "if_present"
        ; (match p with
           | `package v ->
             List [ Atom "package"; Blang.sexp_of_t Package.Predicate.sexp_of_t v ]
           | `public_name v ->
             List
               [ Atom "public_name"; Blang.sexp_of_t Public_name.Predicate.sexp_of_t v ])
        ]
  ;;
end
