(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t =
  { name : string
  ; sexp : Sexp.t
  }

let sexp_of_t { name; sexp = _ } : Sexp.t = Atom name

let read ~sexps_rewriter sexp =
  { name = Dunolinter.Sexp_handler.get_string ~sexps_rewriter sexp; sexp }
;;
