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

(** A handler for fields found in a dune stanza (in files "dune",
    "dune-project", etc.)

    Such field is typically identified by its name, and then it is
    immediately followed by a variable number of arguments.

    For example:

    {v (name dunolint) v}
    {v (libraries base sexps-rewriter) v}

    A field starts with an atom (the field name), and the arguments are
    in a list of sexps.

    By that definition, a stanza is a field itself, and its subfields
    just as well. *)

module type S = Sexp_handler_intf.S

(** A util to create a handler for an atom field, that is one that
    expects exactly one required argument that is an
    [Sexp.Atom]. *)
module Make_atom (_ : sig
    val field_name : string
  end) : S with type t = string

(** A util to create a generic field handler that doesn't try to parse
    its arguments. *)
module Make_sexp_list (_ : sig
    val field_name : string
  end) : S with type t = Sexp.t list

(** A util to create a handler from a single sexpable type. The handler will
    expect exactly one value, which will be parsed and written (or replaced)
    according to the sexp serializer provided. *)
module Make_sexpable
    (_ : sig
       val field_name : string
     end)
    (M : Sexpable.S) : S with type t = M.t

(** A util to create a handler for multiple args with the same sexpable type. The handler will
    expect a list of arguments, which will each be parsed and written (or replaced)
    according to the sexp serializer provided. *)
module Make_sexpable_list
    (_ : sig
       val field_name : string
     end)
    (M : Sexpable.S) : S with type t = M.t list

(** {1 Utils} *)

(** A convenient wrapper for [read] that finds the field to read from
    a list of fields embedded within a containing list of fields. The
    first field with the expected name is chosen and then we call
    [read] on it. *)
val find
  :  (module S with type t = 'a)
  -> sexps_rewriter:Sexps_rewriter.t
  -> fields:Sexp.t list
  -> 'a option

(** Destruct the given field and verify it has the correct field
    name. Return the args then. If the field doesn't have the right
    shape, and error is raised, using a location found via
    [sexps_rewriter]. *)
val get_args
  :  field_name:string
  -> sexps_rewriter:Sexps_rewriter.t
  -> field:Sexp.t
  -> Sexp.t list

(** During a dunolint lint, insert all fields found in [new_fields] that are
    not present in the existing [fields]. For those that are already present,
    it will be necessary to merge them with the expected value, see the
    [rewrite] functions of the module that corresponds to that particular
    field. For example: {!Dune_linter.Library.rewrite}. *)
val insert_new_fields
  :  sexps_rewriter:Sexps_rewriter.t
  -> indicative_field_ordering:string list
  -> fields:Sexp.t list
  -> new_fields:Sexp.t list
  -> unit
