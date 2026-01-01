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

module Error_context = struct
  module Did_you_mean = struct
    type t =
      { var : string
      ; candidates : string list
      }

    let sexp_of_t { var; candidates } =
      Sexp.List
        [ List [ Atom "var"; Atom var ]
        ; List (Atom "candidates" :: List.map (fun s -> Sexp.Atom s) candidates)
        ]
    ;;
  end

  type t =
    { message : string
    ; did_you_mean : Did_you_mean.t option
    ; suggestion : string option
    }

  let sexp_of_t { message; did_you_mean; suggestion } =
    Sexp.List
      (List.concat
         [ [ Sexp.Atom message ]
         ; (match did_you_mean with
            | None -> []
            | Some d -> [ Sexp.List [ Atom "did_you_mean?"; Did_you_mean.sexp_of_t d ] ])
         ; (match suggestion with
            | None -> []
            | Some s -> [ Sexp.List [ Atom "suggestion"; Atom s ] ])
         ])
  ;;

  exception E of t

  let () =
    Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor E] (function
      | E t -> List [ Atom "Dunolint.Sexp_helpers.Error_context.E"; sexp_of_t t ]
      | _ -> assert false)
  ;;

  let message t = t.message
  let did_you_mean t = t.did_you_mean
  let suggestion t = t.suggestion
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

  let candidates t = List.map (fun c -> c.atom) t
end

let parse_variant (type a) (variant_spec : a Variant_spec.t) ~error_source (sexp : Sexp.t)
  : a
  =
  let find_case ~located_sexp atom =
    match
      List.find_opt
        (fun (c : a Variant_spec.case) -> String.equal c.atom atom)
        variant_spec
    with
    | Some case -> case
    | None ->
      let context =
        { Error_context.message = Printf.sprintf "Unknown construct [%s]." atom
        ; did_you_mean =
            Some { var = atom; candidates = Variant_spec.candidates variant_spec }
        ; suggestion = None
        }
      in
      raise (Sexplib0.Sexp_conv.Of_sexp_error (Error_context.E context, located_sexp))
  in
  match sexp with
  | Atom atom ->
    (match find_case ~located_sexp:sexp atom with
     | { conv = Nullary value; _ } -> value
     | { conv = Unary_with_context _ | Unary _ | Variadic _; _ } ->
       let context =
         { Error_context.message =
             Printf.sprintf "The construct [%s] expects one or more arguments." atom
         ; did_you_mean = None
         ; suggestion = Some (Printf.sprintf "Replace by: (%s ARG)" atom)
         }
       in
       raise (Sexplib0.Sexp_conv.Of_sexp_error (Error_context.E context, sexp)))
  | List ((Atom atom as located_sexp) :: args) ->
    (match find_case ~located_sexp atom with
     | { conv = Nullary _; _ } -> Sexplib0.Sexp_conv_error.ptag_no_args error_source sexp
     | { conv = Unary f; _ } ->
       (match args with
        | [ arg ] -> f arg
        | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source atom sexp)
     | { conv = Unary_with_context f; _ } ->
       (match args with
        | [ arg ] -> f ~context:sexp ~arg
        | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source atom sexp)
     | { conv = Variadic f; _ } -> f ~context:sexp ~fields:args)
  | List (List _ :: _) ->
    Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp
  | List [] -> Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp
;;
