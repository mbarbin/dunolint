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

let error_source = "predicate.t"

type t =
  [ `dune_lang_version of Dune_lang_version.Predicate.t Blang.t
  | `generate_opam_files of Generate_opam_files.Predicate.t Blang.t
  | `implicit_transitive_deps of Implicit_transitive_deps.Predicate.t Blang.t
  | `name of Name.Predicate.t Blang.t
  ]

let equal (a : t) (b : t) =
  if phys_equal a b
  then true
  else (
    match a, b with
    | `dune_lang_version va, `dune_lang_version vb ->
      Blang.equal Dune_lang_version.Predicate.equal va vb
    | `generate_opam_files va, `generate_opam_files vb ->
      Blang.equal Generate_opam_files.Predicate.equal va vb
    | `implicit_transitive_deps va, `implicit_transitive_deps vb ->
      Blang.equal Implicit_transitive_deps.Predicate.equal va vb
    | `name va, `name vb -> Blang.equal Name.Predicate.equal va vb
    | ( ( `dune_lang_version _
        | `generate_opam_files _
        | `implicit_transitive_deps _
        | `name _ )
      , _ ) -> false)
;;

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "dune_lang_version"
    ; conv =
        Unary
          (fun sexp ->
            `dune_lang_version
              (Blang.t_of_sexp Dune_lang_version.Predicate.t_of_sexp sexp))
    }
  ; { atom = "generate_opam_files"
    ; conv =
        Unary
          (fun sexp ->
            `generate_opam_files
              (Blang.t_of_sexp Generate_opam_files.Predicate.t_of_sexp sexp))
    }
  ; { atom = "implicit_transitive_deps"
    ; conv =
        Unary
          (fun sexp ->
            `implicit_transitive_deps
              (Blang.t_of_sexp Implicit_transitive_deps.Predicate.t_of_sexp sexp))
    }
  ; { atom = "name"
    ; conv = Unary (fun sexp -> `name (Blang.t_of_sexp Name.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `dune_lang_version v ->
    List
      [ Atom "dune_lang_version"
      ; Blang.sexp_of_t Dune_lang_version.Predicate.sexp_of_t v
      ]
  | `generate_opam_files v ->
    List
      [ Atom "generate_opam_files"
      ; Blang.sexp_of_t Generate_opam_files.Predicate.sexp_of_t v
      ]
  | `implicit_transitive_deps v ->
    List
      [ Atom "implicit_transitive_deps"
      ; Blang.sexp_of_t Implicit_transitive_deps.Predicate.sexp_of_t v
      ]
  | `name v -> List [ Atom "name"; Blang.sexp_of_t Name.Predicate.sexp_of_t v ]
;;
