(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

(* Some functions are copied from [Base] version [v0.17] which is released under
   MIT and may be found at [https://github.com/janestreet/base].

   See Base's LICENSE below:

   ----------------------------------------------------------------------------

   The MIT License

   Copyright (c) 2016--2024 Jane Street Group, LLC <opensource-contacts@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   ----------------------------------------------------------------------------

   When this is the case, we clearly indicate it next to the copied function. *)

module Char = Char0
include Stdlib.StringLabels

let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_string
let t_of_sexp = Sexplib0.Sexp_conv.string_of_sexp

let chop_prefix t ~prefix =
  if starts_with ~prefix t
  then (
    let prefix_len = length prefix in
    Some (sub t ~pos:prefix_len ~len:(length t - prefix_len)))
  else None
;;

let chop_suffix t ~suffix =
  if ends_with ~suffix t
  then Some (sub t ~pos:0 ~len:(length t - length suffix))
  else None
;;

let is_empty t = length t = 0
let is_prefix t ~prefix = starts_with t ~prefix
let is_suffix t ~suffix = ends_with t ~suffix

let lsplit2 t ~on =
  match index_opt t on with
  | None -> None
  | Some i -> Some (sub t ~pos:0 ~len:i, sub t ~pos:(i + 1) ~len:(length t - i - 1))
;;

let split t ~on = split_on_char ~sep:on t
let to_string t = t

let of_char_list cs =
  let buf = Bytes.create (List.length cs) in
  List.iteri (fun i c -> Bytes.set buf i c) cs;
  Bytes.unsafe_to_string buf
;;

(* ---------------------------------------------------------------------------- *)
(* The following functions are copied from [Base] (MIT). See notice at the top
   of the file and project global notice for licensing information. *)

let rfindi t ~f =
  let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
  let pos = length t - 1 in
  (loop pos [@nontail])
;;

let lfindi ?(pos = 0) t ~f =
  let n = length t in
  let rec loop i = if i = n then None else if f i t.[i] then Some i else loop (i + 1) in
  (loop pos [@nontail])
;;

let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c)) [@nontail]
let first_non_drop ~drop t = lfindi t ~f:(fun _ c -> not (drop c)) [@nontail]

let rstrip ?(drop = Char.is_whitespace) t =
  match last_non_drop t ~drop with
  | None -> ""
  | Some i -> if i = length t - 1 then t else sub t ~pos:0 ~len:(i + 1)
;;

let lstrip ?(drop = Char.is_whitespace) t =
  match first_non_drop t ~drop with
  | None -> ""
  | Some 0 -> t
  | Some n -> sub t ~pos:n ~len:(length t - n)
;;

let strip ?drop t =
  match drop with
  | None -> trim t
  | Some drop -> lstrip ~drop (rstrip ~drop t)
;;

(* ---------------------------------------------------------------------------- *)
