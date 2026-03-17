(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Code_error = Code_error0
module List = List0
module Sexp = Sexp0
module With_equal_and_sexp = With_equal_and_sexp0

let phys_equal a b = a == b
let print_s sexp = print_endline (Sexp.to_string_hum sexp)

let require_does_raise f =
  match f () with
  | _ -> failwith "Did not raise."
  | exception e -> print_s (Sexplib0.Sexp_conv.sexp_of_exn e)
;;

let require bool = if not bool then failwith "Required condition does not hold."

let require_equal
      (type a)
      (module M : With_equal_and_sexp.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if not (M.equal v1 v2)
  then
    Code_error.raise
      "Values are not equal."
      [ "v1", v1 |> M.sexp_of_t; "v2", v2 |> M.sexp_of_t ]
;;

let print_endline = print_endline

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Sexplib0.Sexp_conv.Of_sexp_error]
    (function
    | Of_sexp_error (exn, sexp) ->
      let exn =
        match exn with
        | Failure msg -> Sexp.Atom msg
        | _ -> Sexplib0.Sexp_conv.sexp_of_exn exn
      in
      List [ Atom "Of_sexp_error"; exn; List [ Atom "invalid_sexp"; sexp ] ]
    | _ -> assert false)
;;
