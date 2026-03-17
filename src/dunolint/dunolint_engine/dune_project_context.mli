(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Context associated with a [dune-project] file.

    When performing linting it may be useful to refer to context from the
    enclosing dune-project file. This type is used to hold the information we
    wish to access. *)

(** Each value of type [t] refers to context found in exactly one file. This
    type is non mutable. *)
type t

(** [create ~path ~original_contents] parses information needed from a
    ["dune-project"] file, and extracts from it an immutable context required
    for linting. *)
val create : path:Relative_path.t -> original_contents:string -> (t, Err.t) Result.t

(** {1 Getters} *)

(** A valid [dune-project] file will necessarily start with a valid lang dune
    stanza, however if the file is not valid, this may return [None]. *)
val dune_lang_version : t -> Dunolint.Dune_project.Dune_lang_version.t option

module Invalid_dune_project : sig
  (** Logging the actual error must be centralized and done once, so the calling
      code shall simply handle the fact that an error occurred. *)
  type t = private Invalid_dune_project

  val acknowledge : Err.t -> t
end
