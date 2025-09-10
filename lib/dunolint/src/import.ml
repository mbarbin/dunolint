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

open! Stdlib_compat

module Char = struct
  include Char

  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  ;;
end

module Int = struct
  include Int

  let of_string_opt = int_of_string_opt
end

module List = struct
  include ListLabels

  let find t ~f = find_opt t ~f
  let sort t ~compare = sort t ~cmp:compare
end

module String = struct
  include StringLabels

  let t_of_sexp = string_of_sexp
  let sexp_of_t = sexp_of_string
  let is_prefix t ~prefix = String.starts_with ~prefix t
  let is_empty t = String.length t = 0
  let concat ~sep li = String.concat sep li
  let split t ~on = split_on_char ~sep:on t
end

module Option = struct
  include Option

  let t_of_sexp = option_of_sexp
  let sexp_of_t = sexp_of_option
end

let compare_int = Int.compare
let compare_string = String.compare
let compare_list compare a b = List.compare ~cmp:compare a b
let compare_option compare a b = Option.compare compare a b
let equal_int = Int.equal
let equal_string = String.equal
let equal_list eq a b = List.equal ~eq a b
let equal_option eq a b = Option.equal eq a b
