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

type t =
  { src : string
  ; re : Re.re
  }

let to_string t = t.src

let of_string src =
  try
    let t = Re.Glob.glob ~anchored:true ~expand_braces:true ~pathname:true src in
    { src; re = Re.compile t }
  with
  | Re.Glob.Parse_error ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    Stdlib.Printexc.raise_with_backtrace
      (Invalid_argument (Printf.sprintf "Re.Glob.Parse_error: %s" src))
      bt
;;

let v = of_string
let equal t1 t2 = String.equal (to_string t1) (to_string t2)
let test t a = Re.execp t.re a
let t_of_sexp sexp = sexp |> String.t_of_sexp |> of_string
let sexp_of_t t = t |> to_string |> String.sexp_of_t
let compare t1 t2 = String.compare (to_string t1) (to_string t2)
