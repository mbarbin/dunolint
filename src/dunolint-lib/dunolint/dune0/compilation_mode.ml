(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let error_source = "compilation_mode.t"

type t =
  [ `byte
  | `native
  | `best
  | `melange
  ]

let all = ([ `byte; `native; `best; `melange ] : t list)

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "byte"; conv = Nullary `byte }
  ; { atom = "native"; conv = Nullary `native }
  ; { atom = "best"; conv = Nullary `best }
  ; { atom = "melange"; conv = Nullary `melange }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  Atom
    (match t with
     | `byte -> "byte"
     | `native -> "native"
     | `best -> "best"
     | `melange -> "melange")
;;

let to_comparable_int = function
  | `byte -> 0
  | `native -> 1
  | `best -> 2
  | `melange -> 3
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
