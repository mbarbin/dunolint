(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t =
  { message : string
  ; data : (string * Sexp.t) list
  }

exception E of t

let raise message data = raise (E { message; data })

let sexp_of_t { message; data } =
  Sexp.List
    (Atom message :: List.map (fun (field, sexp) -> Sexp.List [ Atom field; sexp ]) data)
;;

let () =
  Printexc.register_printer (function
    | E t -> Some (Sexp.to_string_hum (sexp_of_t t))
    | _ -> None [@coverage off])
;;
