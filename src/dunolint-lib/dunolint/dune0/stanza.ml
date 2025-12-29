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

module Predicate = struct
  [@@@coverage off]

  let error_source = "stanza.t"

  type t =
    [ `include_subdirs
    | `library
    | `executable
    | `executables
    ]

  let equal = (Stdlib.( = ) : t -> t -> bool)

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__006_ as _sexp__008_ ->
       (match atom__006_ with
        | "include_subdirs" -> `include_subdirs
        | "library" -> `library
        | "executable" -> `executable
        | "executables" -> `executables
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__006_ :: _) as _sexp__008_ ->
       (match atom__006_ with
        | "include_subdirs" ->
          Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__008_
        | "library" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__008_
        | "executable" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__008_
        | "executables" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__008_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__007_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__007_
     | Sexplib0.Sexp.List [] as sexp__007_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__007_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__011_ ->
       try __t_of_sexp__ sexp__011_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__011_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `include_subdirs -> Sexplib0.Sexp.Atom "include_subdirs"
     | `library -> Sexplib0.Sexp.Atom "library"
     | `executable -> Sexplib0.Sexp.Atom "executable"
     | `executables -> Sexplib0.Sexp.Atom "executables"
     : t -> Sexplib0.Sexp.t)
  ;;
end
