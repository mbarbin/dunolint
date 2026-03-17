(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

let error_source = "predicate.t"

type t =
  [ `path of Path.Predicate.t Blang.t
  | `dune of Dune.Predicate.t Blang.t
  | `dune_project of Dune_project.Predicate.t Blang.t
  | `dune_workspace of Dune_workspace.Predicate.t Blang.t
  | `dunolint of Dunolint0.Predicate.t Blang.t
  ]

let equal (a : t) (b : t) =
  if phys_equal a b
  then true
  else (
    match a, b with
    | `path va, `path vb -> Blang.equal Path.Predicate.equal va vb
    | `dune va, `dune vb -> Blang.equal Dune.Predicate.equal va vb
    | `dune_project va, `dune_project vb -> Blang.equal Dune_project.Predicate.equal va vb
    | `dune_workspace va, `dune_workspace vb ->
      Blang.equal Dune_workspace.Predicate.equal va vb
    | `dunolint va, `dunolint vb -> Blang.equal Dunolint0.Predicate.equal va vb
    | (`path _ | `dune _ | `dune_project _ | `dune_workspace _ | `dunolint _), _ -> false)
;;

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "path"
    ; conv = Unary (fun sexp -> `path (Blang.t_of_sexp Path.Predicate.t_of_sexp sexp))
    }
  ; { atom = "dune"
    ; conv = Unary (fun sexp -> `dune (Blang.t_of_sexp Dune.Predicate.t_of_sexp sexp))
    }
  ; { atom = "dune_project"
    ; conv =
        Unary
          (fun sexp ->
            `dune_project (Blang.t_of_sexp Dune_project.Predicate.t_of_sexp sexp))
    }
  ; { atom = "dune_workspace"
    ; conv =
        Unary
          (fun sexp ->
            `dune_workspace (Blang.t_of_sexp Dune_workspace.Predicate.t_of_sexp sexp))
    }
  ; { atom = "dunolint"
    ; conv =
        Unary (fun sexp -> `dunolint (Blang.t_of_sexp Dunolint0.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `path v -> List [ Atom "path"; Blang.sexp_of_t Path.Predicate.sexp_of_t v ]
  | `dune v -> List [ Atom "dune"; Blang.sexp_of_t Dune.Predicate.sexp_of_t v ]
  | `dune_project v ->
    List [ Atom "dune_project"; Blang.sexp_of_t Dune_project.Predicate.sexp_of_t v ]
  | `dune_workspace v ->
    List [ Atom "dune_workspace"; Blang.sexp_of_t Dune_workspace.Predicate.sexp_of_t v ]
  | `dunolint v ->
    List [ Atom "dunolint"; Blang.sexp_of_t Dunolint0.Predicate.sexp_of_t v ]
;;
