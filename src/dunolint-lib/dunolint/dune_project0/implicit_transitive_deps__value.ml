(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t =
  [ `True
  | `False
  | `False_if_hidden_includes_supported
  ]

let all = ([ `True; `False; `False_if_hidden_includes_supported ] : t list)

let of_string = function
  | "true" -> Some `True
  | "false" -> Some `False
  | "false-if-hidden-includes-supported" -> Some `False_if_hidden_includes_supported
  | _ -> None
;;

let to_string = function
  | `True -> "true"
  | `False -> "false"
  | `False_if_hidden_includes_supported -> "false-if-hidden-includes-supported"
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp sexp =
  match (sexp : Sexp.t) with
  | Sexp.Atom s ->
    (match of_string s with
     | Some v -> v
     | None ->
       raise
         (Sexp.Of_sexp_error
            ( Failure (Printf.sprintf "Unsupported implicit_transitive_deps value [%s]." s)
            , sexp )))
  | _ ->
    raise
      (Sexp.Of_sexp_error
         (Failure "Expected atom for implicit_transitive_deps value.", sexp))
;;

let to_comparable_int = function
  | `True -> 0
  | `False -> 1
  | `False_if_hidden_includes_supported -> 2
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
