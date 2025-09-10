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

let parsing_config_version_0 = ref false

let when_parsing_config_version_0 ~f =
  let init = parsing_config_version_0.contents in
  Fun.protect
    (fun () ->
       parsing_config_version_0 := true;
       f ())
    ~finally:(fun () -> parsing_config_version_0 := init)
;;

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
    | [ (List (List _ :: _) as list) ] ->
      if !parsing_config_version_0
      then list
      else
        Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
          error_source
          tag
          context [@coverage off]
        (* out edge bisect_ppx issue. *)
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
