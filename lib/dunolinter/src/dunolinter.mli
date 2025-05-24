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

module Enforce_result = Enforce_result
module Handler = Handler
module Linter = Linter
module Linters = Linters
module Sexp_handler = Sexp_handler
module Stanza_linter = Stanza_linter

module Stanza : sig
  (** The type that is iterated on during linting. Stanzas that are
      actively parsed by this library will be provided to you along with
      typed handlers. In all cases, the original sexp can be accessed to
      support low-level rewrites with the [Sexps_rewriter] API. *)
  type 'a t
end

module type S = Dunolinter_intf.S with type 'a stanza := 'a Stanza.t

(** Returns the path of the file where the stanza was located when it was
    loaded. *)
val path : _ Stanza.t -> Relative_path.t

(** Stanzas that do not have support for predicate enforcement shall be ignored
    when using this interface, they will be left untouched by dunolint. *)
val linter : _ Stanza.t -> Linter.t

(** If you are writing a custom rewriter perhaps you'll find it easier to use
    directly the stanza API provided by a linter library, in which case you'll
    need this accessor. In this use of the library, you perform side effects to
    ['a] directly using the linter api, rather than going through the
    enforcement of configurable conditions. *)
val match_stanza : 'a Stanza.t -> 'a

(** That is the [sexp] that correspond to the entire stanza held by
    this rewriter. These are the sexps found at the top level of dune
    files, for example [(library ..)] or [(executable ..)].

    This accessor is provided if you desire to implement a low-level
    rewriter using [Sexps_rewriter]. *)
val original_sexp : _ Stanza.t -> Sexp.t

(** That is the [Sexps_rewriter] that was created to read the stanza
    held by this rewriter. *)
val sexps_rewriter : _ Stanza.t -> Sexps_rewriter.t

(** A shared util to evaluate a predicate on paths. This is used to evaluate the
    skip_subtree part of a dunolint config. *)
val eval_path
  :  path:Relative_path.t
  -> condition:Dunolint.Path.Predicate.t Blang.t
  -> Dunolint.Trilang.t

module Private : sig
  module Stanza : sig
    module For_create : sig
      type 'a t =
        { stanza : 'a
        ; path : Relative_path.t
        ; original_sexp : Sexp.t
        ; sexps_rewriter : Sexps_rewriter.t
        ; linter : Linter.t
        }
    end

    val create : 'a For_create.t -> 'a Stanza.t
  end
end
