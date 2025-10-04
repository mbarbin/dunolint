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

module type S = sig
  (** A linter for a file containing stanzas.

      A value of type [t] holds enough capability to handle in
      sequence the linting of all the stanzas contained in a single
      file. *)

  (** The type t will usually be mutable. A typical life cycle for it
      is to be created, then some side effects are performed on it due
      to the application of linting rules. At the end, we create the
      linted contents of the result. *)
  type t

  (** This type will be available as ['a Dunolinter.Stanza.t] however it
      is needed here in this interface to avoid a circular dependency.
      This is the type for the stanza that will be iterated on during
      linting. *)
  type 'a stanza

  (** {1 Stanzas} *)

  module Stanza : sig
    (** [t] is extensible so it is easier to extend it in future versions
        of [dunolint] while maintaining better compatibility with user
        code, which shall embrace the fact that they shall handle stanzas
        that are not parsed by the tool. *)
    type t = private ..
  end

  (** {1 Creation} *)

  (** A [t] shall be created from a string that would yield a successful parsing
      of many sexps and positions from a dune file, using the [Parsexp] library.
      The [path] is provided to create a [Loc.t] for error messages, but no I/O
      is actually performed on disk - the sexps are parsed from the string
      provided by the parameter [original_contents].

      The type is intended to make it easy to connect to code using
      [Err], such as shown below:

      {[
        match Dune_linter.create ~path ~original_contents with
        | Ok r -> r
        | Error { loc; message } -> Err.raise ~loc [ Pp.text message ]
      ]} *)
  val create
    :  path:Relative_path.t
    -> original_contents:string
    -> (t, Sexps_rewriter.Parse_error.t) Result.t

  (** {1 Linting} *)

  (** Iter through the stanzas found in the file. The function [f]
      provided will be called on all the toplevel stanzas found in the
      file.

      The expected way to use this function is to do some pattern
      matching on the [Stanza.t] currently at hand, and use the
      corresponding api if some rewrites are required. If the current
      stanza is not one you are targeting you may simply ignore and
      return unit, to be called again on the remaining stanzas of the
      input.

      Note that you may visit the same file multiple times (that is,
      calling [visit] multiple times with different invocations of [f]),
      however be mindful that the [file_rewriter] that you manipulate is
      the same each time, thus the final computation of the output will
      fail if you enqueue incompatible rewrites in it. This is however
      not a recommended way to use this library, as this may be quite
      confusing. Indeed, the state you will match on will be that of the
      original contents, even though some rewrites have already happen.
      It shall be preferred to apply all the rewrites you wish in one
      path for each stanza, unless you are sure the rewrites are
      targeting parts of the input that are clearly independent.

      If you need to apply low-level rewrites, you may do so with the
      [Sexps_rewriter] api, by accessing the [original_sexp] of the
      stanza. For example, you can do so with stanzas or rewrites that
      are not natively supported by this library. *)
  val visit : t -> f:(Stanza.t stanza -> unit) -> unit

  (** {1 Output} *)

  (** Produce the resulting buffer, with all the rewrites applied. Note that [t]
      may continue to be used further from here, and you can call [contents]
      again later. This raises [File_rewriter.Invalid_rewrites] if
      inconsistent rewrites have been submitted to t's [file_rewriter].

      Note that this library is a rather low-level util library. In
      particular, using this library does not get you auto-formatting of
      dune files, thus the output may be not particularly pretty or well
      indented. For a more ergonomic wrapper, and inclusion of auto-fmt,
      you should consider using via [Dunolint_engine]. *)
  val contents : t -> string

  (** {1 Getters} *)

  (** If you need access to the internal sexps_rewriter, this accessor is
      exposed. *)
  val sexps_rewriter : t -> Sexps_rewriter.t

  (** Retrieve the path provided when [t] was created. *)
  val path : t -> Relative_path.t
end
