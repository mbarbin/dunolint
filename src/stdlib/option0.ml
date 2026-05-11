(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

include Stdlib.Option

let iter t ~f = iter f t
let map t ~f = map f t

let value_map t ~default ~f =
  match t with
  | None -> default
  | Some v -> f v
;;

let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_option
