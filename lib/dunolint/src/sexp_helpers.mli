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

(** Some helpers used by sexp serializers. *)

module type T_of_sexp = sig
  type t

  val t_of_sexp : Sexp.t -> t
end

(** During the transition to support version 0 of the config, we allow wrapped
    records for older formats. Once we'll be done migrating to version 1, we can
    retire this. This is [false] by default and requires to be set to [true] to
    parse version 0. *)
val parsing_config_version_0 : bool ref

(** Temporarily set [parsing_config_version_0] to true for the execution of [f]
    (protected). *)
val when_parsing_config_version_0 : f:(unit -> 'a) -> 'a

(** When a record is embedded by a variant or polymorphic variant we'd like to
    support a syntax with less parens around. For example:

    Suppose you have a record type M:

    {[
      module M = struct
        type t =
          { a : string
          ; b : int
          }
      end

      type t = [ `cons of M.t ]
    ]}

    We'd like to parse:

    {[
      cons (a hello) (b 42)
    ]}

    Instead of:

    {[
      cons ((a hello) (b 42))
    ]}

    However care must be applied for the parsing exceptions raised by use an
    actual sexp of the input, otherwise there would be no location. [context] is
    the sexp used to error out, and [fields] the record fields. M is able to
    parse the fields when they are wrapped by a [Sexp.List] constructor. *)
val parse_inline_record
  :  (module T_of_sexp with type t = 'a)
  -> error_source:string
  -> context:Sexp.t
  -> tag:string
  -> fields:Sexp.t list
  -> 'a
