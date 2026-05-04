(***************************************************************************************)
(*  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(***************************************************************************************)

(* Notice: parts of this file are vendored from [Stdio.Out_channel], which we
   documented in the NOTICE at the root of the repo. The original license is
   reproduced in [third-party-license/janestreet/stdio] and below. Each
   binding is annotated below with its provenance. *)

(* The MIT License

   Copyright (c) 2016--2024 Jane Street Group, LLC
   <opensource-contacts@janestreet.com>

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
   SOFTWARE. *)

include Stdlib.Out_channel

(* Vendored verbatim from [Stdio.Out_channel]. *)

let newline t = output_string t "\n"

let output_line t line =
  output_string t line;
  newline t
;;

(* ====================================================================== *)

(* Below is not vendored verbatim, project-specific or customized. *)

(* Adapted from [Stdio.Out_channel] with the [?binary] and [?fail_if_exists]
   parameters dropped — neither is used in the project. [Open_binary] is
   hardcoded; [Open_excl] is no longer reachable. *)
let create ?(append = false) ?(perm = 0o666) file =
  let flags = [ Open_wronly; Open_creat; Open_binary ] in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  open_gen flags perm file
;;

(* Raised by [with_file] when [f] raises and the subsequent close also
   raises — so that neither error is lost. Intentionally not exposed in the
   [.mli]: callers are expected to re-raise as-is and rely on the registered
   [Sexp_conv] for diagnostics, rather than pattern-matching to recover the
   two underlying exceptions. *)
exception
  With_file_exn of
    { user : exn
    ; close : exn
    }

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor With_file_exn] (function
    | With_file_exn { user; close } ->
      List
        [ Atom "Out_channel.with_file failed."
        ; List [ Atom "user"; Sexplib0.Sexp_conv.sexp_of_exn user ]
        ; List [ Atom "close"; Sexplib0.Sexp_conv.sexp_of_exn close ]
        ]
    | _ -> assert false)
;;

let with_file ?append ?perm file ~f =
  let t = create ?append ?perm file in
  match f t with
  | v ->
    close t;
    v
  | exception user_exn ->
    let bt = Printexc.get_raw_backtrace () in
    (match close t with
     | () -> Printexc.raise_with_backtrace user_exn bt
     | exception close_exn ->
       Printexc.raise_with_backtrace
         (With_file_exn { user = user_exn; close = close_exn })
         bt)
;;

(* [write_all]'s source happens to be identical to Stdio's but depends on the local
   [with_file]. *)
let write_all filename ~data = with_file filename ~f:(fun t -> output_string t data)
