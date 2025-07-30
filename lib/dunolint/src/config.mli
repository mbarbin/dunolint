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

type t [@@deriving compare, equal, sexp]

(** {1 Getters} *)

(** {2 Skip subtree}

    This part relate to making dunolint ignor parts of your project,
    namely not visiting entire sub directories of your repo. *)

module Skip_subtree : sig
  module Predicate : sig
    type t = [ `path of Path.Predicate.t Blang.t ] [@@deriving compare, equal, sexp]
  end

  module Result : sig
    type t = Nothing.t [@@deriving compare, equal, sexp]
  end

  type t = (Predicate.t, Result.t) Rule.t [@@deriving compare, equal, sexp]
end

val skip_subtree : t -> Skip_subtree.t option

(** {2 Generic rules} *)

module Rule : sig
  type t = (Predicate.t, Condition.t) Rule.t [@@deriving compare, equal, sexp]
end

val rules : t -> Rule.t list

(** {1 Creating configs} *)

val create : ?skip_subtree:Skip_subtree.t -> ?rules:Rule.t list -> unit -> t

(** {1 An EDSL to build configs} *)

module Std : sig
  module Blang = Blang
  module Dune = Dune
  module Dune_project = Dune_project

  include module type of struct
    include Blang.O
  end

  val backend : 'a -> [> `backend of 'a ] Blang.t
  val cond : ('condition * 'action) list -> [> `cond of ('condition * 'action) list ]
  val dune : 'a -> [> `dune of 'a ] Blang.t
  val dune_project : 'a -> [> `dune_project of 'a ] Blang.t
  val enforce : 'a -> [> `enforce of 'a ]
  val equals : 'a -> [> `equals of 'a ] Blang.t
  val executable : 'a -> [> `executable of 'a ] Blang.t
  val flag : Dune.Pps.Predicate.Flag.t -> [> `flag of Dune.Pps.Predicate.Flag.t ] Blang.t
  val generate_opam_files : 'a -> [> `generate_opam_files of 'a ] Blang.t
  val glob : string -> [> `glob of Glob.t ] Blang.t
  val has_field : 'a -> [> `has_field of 'a ] Blang.t
  val has_mode : 'a -> [> `has_mode of 'a ] Blang.t
  val has_modes : 'a -> [> `has_modes of 'a ] Blang.t
  val implicit_transitive_deps : 'a -> [> `implicit_transitive_deps of 'a ] Blang.t
  val include_subdirs : 'a -> [> `include_subdirs of 'a ] Blang.t
  val instrumentation : 'a -> [> `instrumentation of 'a ] Blang.t
  val is_prefix : string -> [> `is_prefix of string ] Blang.t
  val is_present : [> `is_present ] Blang.t
  val is_suffix : string -> [> `is_suffix of string ] Blang.t
  val library : 'a -> [> `library of 'a ] Blang.t
  val lint : 'a -> [> `lint of 'a ] Blang.t
  val modes : 'a -> [> `modes of 'a ] Blang.t
  val name : 'a -> [> `name of 'a ] Blang.t
  val no_preprocessing : [> `no_preprocessing ] Blang.t
  val path : 'a -> [> `path of 'a ] Blang.t
  val pp : Dune.Pp.Name.t -> [> `pp of Dune.Pp.Name.t ] Blang.t
  val pps : 'a -> [> `pps of 'a ] Blang.t

  val pp_with_flag
    :  Dune.Pps.Predicate.Pp_with_flag.t
    -> [> `pp_with_flag of Dune.Pps.Predicate.Pp_with_flag.t ] Blang.t

  val preprocess : 'a -> [> `preprocess of 'a ] Blang.t
  val public_name : 'a -> [> `public_name of 'a ] Blang.t
  val return : [> `return ]
  val skip_subtree : [> `skip_subtree ]
  val stanza : 'a -> [> `stanza of 'a ] Blang.t
end
