(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*_********************************************************************************)

open! Stdlib_compat

module Char : sig
  include module type of Char

  val is_alphanum : char -> bool
end

module Int : sig
  include module type of Int

  val of_string_opt : string -> int option
end

module List : sig
  include module type of ListLabels

  val find : 'a list -> f:('a -> bool) -> 'a option
  val sort : 'a list -> compare:('a -> 'a -> int) -> 'a list
end

module String : sig
  include module type of StringLabels

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val concat : sep:string -> string list -> string
  val is_prefix : string -> prefix:string -> bool
  val is_empty : string -> bool
  val split : string -> on:char -> string list
end

module Option : sig
  include module type of Option

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end

val compare_int : int -> int -> int
val compare_string : string -> string -> int
val compare_list : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val compare_option : ('a -> 'a -> int) -> 'a option -> 'a option -> int
val equal_int : int -> int -> bool
val equal_string : string -> string -> bool
val equal_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val equal_option : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
