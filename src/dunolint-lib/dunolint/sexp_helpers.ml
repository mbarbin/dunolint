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

module type T_of_sexp = sig
  type t

  val t_of_sexp : Sexp.t -> t
end

let parse_inline_record
      (type a)
      (module M : T_of_sexp with type t = a)
      ~error_source
      ~context
      ~tag
      ~fields
  =
  let arg =
    match (fields : Sexp.t list) with
    | [ List (List _ :: _) ] ->
      Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
        error_source
        tag
        context [@coverage off] (* out edge bisect_ppx issue. *)
    | list -> Sexp.List list
  in
  match M.t_of_sexp arg with
  | ok -> ok
  | exception Sexplib0.Sexp.Of_sexp_error (exn, _) ->
    let bt = Printexc.get_raw_backtrace () in
    (Printexc.raise_with_backtrace
       (Sexplib0.Sexp.Of_sexp_error (exn, context))
       bt [@coverage off] (* out edge bisect_ppx issue. *))
;;

module Variant_spec = struct
  type 'a conv =
    | Nullary of 'a
    | Unary_with_context of (context:Sexp.t -> arg:Sexp.t -> 'a)
    | Unary of (Sexp.t -> 'a)
    | Variadic of (context:Sexp.t -> fields:Sexp.t list -> 'a)

  type 'a case =
    { atom : string
    ; conv : 'a conv
    }

  type 'a t = 'a case list
end

let parse_variant (type a) (variant_spec : a Variant_spec.t) ~error_source (sexp : Sexp.t)
  : a
  =
  let find_case atom =
    List.find_opt (fun (c : a Variant_spec.case) -> String.equal c.atom atom) variant_spec
  in
  match sexp with
  | Atom atom ->
    (match find_case atom with
     | Some { conv = Nullary value; _ } -> value
     | Some { conv = Unary_with_context _ | Unary _ | Variadic _; _ } ->
       Sexplib0.Sexp_conv_error.ptag_takes_args error_source sexp
     | None -> Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp)
  | List (Atom atom :: args) ->
    (match find_case atom with
     | None -> Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp
     | Some { conv = Nullary _; _ } ->
       Sexplib0.Sexp_conv_error.ptag_no_args error_source sexp
     | Some { conv = Unary f; _ } ->
       (match args with
        | [ arg ] -> f arg
        | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source atom sexp)
     | Some { conv = Unary_with_context f; _ } ->
       (match args with
        | [ arg ] -> f ~context:sexp ~arg
        | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source atom sexp)
     | Some { conv = Variadic f; _ } -> f ~context:sexp ~fields:args)
  | List (List _ :: _) ->
    Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp
  | List [] -> Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp
;;
