(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Predicate : sig
  module Param : sig
    type t =
      [ `any
      | `none
      | `some
      | `equals of string
      ]

    val equal : t -> t -> bool

    include Sexpable.S with type t := t
  end

  module Flag : sig
    module Applies_to : sig
      type t =
        [ `any
        | `driver
        | `pp of Pp.Name.t
        ]

      val equal : t -> t -> bool

      include Sexpable.S with type t := t
    end

    type t =
      { name : string
      ; param : Param.t
      ; applies_to : Applies_to.t
      }

    val equal : t -> t -> bool

    include Sexpable.S with type t := t
  end

  module Pp_with_flag : sig
    type t =
      { pp : Pp.Name.t
      ; flag : string
      ; param : Param.t
      }

    val equal : t -> t -> bool

    include Sexpable.S with type t := t
  end

  type t =
    [ `pp of Pp.Name.t
    | `flag of Flag.t
    | `pp_with_flag of Pp_with_flag.t
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
