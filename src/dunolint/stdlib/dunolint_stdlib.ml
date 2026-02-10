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

let phys_equal a b = a == b

module Sexp = Sexplib0.Sexp

module With_equal_and_sexp = struct
  module type S = sig
    type t

    val equal : t -> t -> bool
    val sexp_of_t : t -> Sexp.t
  end
end

module Code_error = struct
  type t =
    { message : string
    ; data : (string * Sexp.t) list
    }

  exception E of t

  let raise message data = raise (E { message; data })

  let sexp_of_t { message; data } =
    Sexp.List
      (Atom message :: List.map (fun (field, sexp) -> Sexp.List [ Atom field; sexp ]) data)
  ;;

  let () =
    Printexc.register_printer (function
      | E t -> Some (Sexp.to_string_hum (sexp_of_t t))
      | _ -> None [@coverage off])
  ;;
end

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
