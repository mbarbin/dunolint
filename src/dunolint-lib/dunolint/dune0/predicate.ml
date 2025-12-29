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

[@@@coverage off]

let error_source = "predicate.t"

module Has_field = struct
  type t =
    [ `instrumentation
    | `lint
    | `name
    | `preprocess
    | `public_name
    ]

  let equal = (Stdlib.( = ) : t -> t -> bool)

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "instrumentation"; conv = Nullary `instrumentation }
    ; { atom = "lint"; conv = Nullary `lint }
    ; { atom = "name"; conv = Nullary `name }
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
       | `instrumentation -> "instrumentation"
       | `lint -> "lint"
       | `name -> "name"
       | `preprocess -> "preprocess"
       | `public_name -> "public_name")
  ;;
end

type t =
  [ `executable of Executable.Predicate.t Blang.t
  | `has_field of Has_field.t
  | `include_subdirs of Include_subdirs.Predicate.t Blang.t
  | `instrumentation of Instrumentation.Predicate.t Blang.t
  | `library of Library.Predicate.t Blang.t
  | `lint of Lint.Predicate.t Blang.t
  | `preprocess of Preprocess.Predicate.t Blang.t
  | `stanza of Stanza.Predicate.t Blang.t
  ]

let equal (a : t) (b : t) =
  if Stdlib.( == ) a b
  then true
  else (
    match a, b with
    | `executable va, `executable vb -> Blang.equal Executable.Predicate.equal va vb
    | `has_field va, `has_field vb -> Has_field.equal va vb
    | `include_subdirs va, `include_subdirs vb ->
      Blang.equal Include_subdirs.Predicate.equal va vb
    | `instrumentation va, `instrumentation vb ->
      Blang.equal Instrumentation.Predicate.equal va vb
    | `library va, `library vb -> Blang.equal Library.Predicate.equal va vb
    | `lint va, `lint vb -> Blang.equal Lint.Predicate.equal va vb
    | `preprocess va, `preprocess vb -> Blang.equal Preprocess.Predicate.equal va vb
    | `stanza va, `stanza vb -> Blang.equal Stanza.Predicate.equal va vb
    | ( ( `executable _
        | `has_field _
        | `include_subdirs _
        | `instrumentation _
        | `library _
        | `lint _
        | `preprocess _
        | `stanza _ )
      , _ ) -> false)
;;

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "executable"
    ; conv =
        Unary
          (fun sexp -> `executable (Blang.t_of_sexp Executable.Predicate.t_of_sexp sexp))
    }
  ; { atom = "has_field"
    ; conv = Unary (fun sexp -> `has_field (Has_field.t_of_sexp sexp))
    }
  ; { atom = "include_subdirs"
    ; conv =
        Unary
          (fun sexp ->
            `include_subdirs (Blang.t_of_sexp Include_subdirs.Predicate.t_of_sexp sexp))
    }
  ; { atom = "instrumentation"
    ; conv =
        Unary
          (fun sexp ->
            `instrumentation (Blang.t_of_sexp Instrumentation.Predicate.t_of_sexp sexp))
    }
  ; { atom = "library"
    ; conv =
        Unary (fun sexp -> `library (Blang.t_of_sexp Library.Predicate.t_of_sexp sexp))
    }
  ; { atom = "lint"
    ; conv = Unary (fun sexp -> `lint (Blang.t_of_sexp Lint.Predicate.t_of_sexp sexp))
    }
  ; { atom = "preprocess"
    ; conv =
        Unary
          (fun sexp -> `preprocess (Blang.t_of_sexp Preprocess.Predicate.t_of_sexp sexp))
    }
  ; { atom = "stanza"
    ; conv = Unary (fun sexp -> `stanza (Blang.t_of_sexp Stanza.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `executable v ->
    List [ Atom "executable"; Blang.sexp_of_t Executable.Predicate.sexp_of_t v ]
  | `has_field v -> List [ Atom "has_field"; Has_field.sexp_of_t v ]
  | `include_subdirs v ->
    List [ Atom "include_subdirs"; Blang.sexp_of_t Include_subdirs.Predicate.sexp_of_t v ]
  | `instrumentation v ->
    List [ Atom "instrumentation"; Blang.sexp_of_t Instrumentation.Predicate.sexp_of_t v ]
  | `library v -> List [ Atom "library"; Blang.sexp_of_t Library.Predicate.sexp_of_t v ]
  | `lint v -> List [ Atom "lint"; Blang.sexp_of_t Lint.Predicate.sexp_of_t v ]
  | `preprocess v ->
    List [ Atom "preprocess"; Blang.sexp_of_t Preprocess.Predicate.sexp_of_t v ]
  | `stanza v -> List [ Atom "stanza"; Blang.sexp_of_t Stanza.Predicate.sexp_of_t v ]
;;
