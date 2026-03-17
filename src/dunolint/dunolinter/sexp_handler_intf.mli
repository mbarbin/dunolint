(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module type S = sig
  (** [t] is type OCaml representation of what the field encodes. It does not
      need to encapsulate the complete information that code be represented by
      dune, only that part that dunolint wants to lint. *)
  type t

  val field_name : string

  (** If the stanza already exists, parse the value currently present.
      The [sexps_rewriter] is given in order to find locations of the supplied
      sexp to supply to the error messages. Errors are raised if the arguments
      do not have the expected shape for that field. *)
  val read : sexps_rewriter:Sexps_rewriter.t -> field:Sexp.t -> t

  (** Write as a new field. This is used to create the field the first time it
      is introduced in a stanza, such as when a new dune file is created by
      dunolint. *)
  val write : t -> Sexp.t

  (** When the field is already present, it may be necessary to merge the
      existing values with the expected one. In this case we do not write the
      field from scratch, instead we refactor it using a rewrite. The [field]
      sexp is expected to be taken from the [sexps_rewriter] and must be the
      original sexp that represent the entire field.

      If the stanza already exists, all we do is some linting, that is we update
      it if needed, and perhaps generating some errors or warnings along the
      way. To be called with the sexps that follow the "(library)" atom, which
      are labeled "fields".

      This has the effect of side-effecting the [File_rewriter] that is
      contained by the [sexps_rewriter] parameter. For this call to be useful, it
      is assumed that the caller is going to output the resulting rewrite, and do
      something with it. The [field]
      sexp is expected to be taken from the [sexps_rewriter] and must be the
      original sexp that represent the entire field. *)
  val rewrite : t -> sexps_rewriter:Sexps_rewriter.t -> field:Sexp.t -> unit
end
