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

type t

val equal : t -> t -> bool
val compare : t -> t -> int

(** This is used by tests for quick debug. To print the config into a file, see
    {!val:to_file_contents}. *)
val sexp_of_t : t -> Sexp.t

module V0 = Config_v0
module V1 = Config_v1

(** {1 Create} *)

val v0 : V0.t -> t
val v1 : V1.t -> t

(** {1 Save to file} *)

(** This is the recommended way to create the contents of the config to save to
    a file via a dune rule. [generated_by] should be the path to the file that
    implements the config, and will be mentioned in a header comment at the top
    with a sentence indicating that the config is generated and should not be
    edited. *)
val to_file_contents : t -> generated_by:string -> string

(** To/from stanzas. *)

val of_stanzas : Sexp.t list -> t
val to_stanzas : t -> Sexp.t list

(** {1 Private Utils} *)

module Private : sig
  val view : t -> [ `v0 of V0.t | `v1 of V1.t ]
end

(** {1 Compatibility}

    We plan on removing this compatibility layer and enforcing the use of the
    versioned API in the future. This will be done as a gradual and multi steps
    transition. At the moment we offer both APIs to start experimenting with the
    specification of configs using the versioned API. *)

module Skip_subtree = Config_v0.Skip_subtree
module Rule = Config_v0.Rule
module Std = Config_v0.Std

val skip_subtree : t -> Skip_subtree.t option
val rules : t -> Rule.t list
val create : ?skip_subtree:Skip_subtree.t -> ?rules:Rule.t list -> unit -> t
