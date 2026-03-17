(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Backend : sig
  module Name : sig
    type t

    include Container_key.S with type t := t
    include Validated_string.S with type t := t
  end

  module Flag : sig
    type t = string

    val equal : t -> t -> bool

    include Sexpable.S with type t := t
  end

  type t

  val create : name:Name.t -> flags:Flag.t list -> t
  val v : ?flags:Flag.t list -> string -> t
  val name : t -> Name.t
  val flags : t -> Flag.t list
  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end

module Predicate : sig
  type t = [ `backend of Backend.t ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
