(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

type 'a t = 'a Dunolint.Nonempty_list.t = ( :: ) of 'a * 'a list

let to_list (hd :: tl : _ t) : _ list = hd :: tl
let iter t ~f = List0.iter (to_list t) ~f
let for_all t ~f = List0.for_all (to_list t) ~f
