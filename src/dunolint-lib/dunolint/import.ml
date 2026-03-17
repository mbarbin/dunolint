(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Stdlib_compat

let phys_equal a b = a == b

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
  let concat ~sep li = String.concat sep li
  let is_prefix t ~prefix = String.starts_with ~prefix t
  let is_empty t = String.length t = 0
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
let equal_int = Int.equal
let equal_string = String.equal
let equal_list eq a b = List.equal ~eq a b
