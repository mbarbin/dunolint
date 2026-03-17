(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let error_source = "linted_file_kind.t"

type t =
  [ `dune
  | `dune_project
  | `dune_workspace
  | `dunolint
  ]

let all = ([ `dune; `dune_project; `dune_workspace; `dunolint ] : t list)

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "dune"; conv = Nullary `dune }
  ; { atom = "dune_project"; conv = Nullary `dune_project }
  ; { atom = "dune_workspace"; conv = Nullary `dune_workspace }
  ; { atom = "dunolint"; conv = Nullary `dunolint }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `dune -> Atom "dune"
  | `dune_project -> Atom "dune_project"
  | `dune_workspace -> Atom "dune_workspace"
  | `dunolint -> Atom "dunolint"
;;

let to_string = function
  | `dune -> "dune"
  | `dune_project -> "dune-project"
  | `dune_workspace -> "dune-workspace"
  | `dunolint -> "dunolint"
;;

let of_string = function
  | "dune" -> Ok `dune
  | "dune-project" -> Ok `dune_project
  | "dune-workspace" -> Ok `dune_workspace
  | "dunolint" -> Ok `dunolint
  | str -> Error (`Msg (Printf.sprintf "Invalid linted file kind: %S" str))
;;

let to_comparable_int = function
  | `dune -> 0
  | `dune_project -> 1
  | `dune_workspace -> 2
  | `dunolint -> 3
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
