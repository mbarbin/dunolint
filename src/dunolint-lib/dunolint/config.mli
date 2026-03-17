(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val equal : t -> t -> bool

(** This is used by tests for quick debug. To print the config into a file, see
    {!val:to_file_contents}. *)
val sexp_of_t : t -> Sexp.t

module V1 = Config_v1

(** {1 Create} *)

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

(** Helpers. *)

module Std = Edsl_std

(** {1 Private Utils} *)

module Private : sig
  val view : t -> [ `v1 of V1.t ]
end

(** {1 Compatibility}

    We plan on removing this compatibility layer and enforcing the use of the
    versioned API in the future. This will be done as a gradual and multi steps
    transition. At the moment we offer both APIs to start experimenting with the
    specification of configs using the versioned API. *)

module Rule = Config_v1.Rule

val create : ?rules:Rule.t list -> unit -> t
