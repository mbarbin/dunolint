(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

module Predicate = struct
  module Param = struct
    type t =
      [ `any
      | `none
      | `some
      | `equals of string
      ]
    [@@deriving compare, equal, sexp]
  end

  module Flag = struct
    module Applies_to = struct
      type t =
        [ `any
        | `driver
        | `pp of Pp.Name.t
        ]
      [@@deriving compare, equal, sexp]
    end

    type t =
      { name : string
      ; param : Param.t
      ; applies_to : Applies_to.t
      }
    [@@deriving compare, equal, sexp]
  end

  module Pp_with_flag = struct
    type t =
      { pp : Pp.Name.t
      ; flag : string
      ; param : Param.t
      }
    [@@deriving compare, equal, sexp]
  end

  type t =
    [ `pp of Pp.Name.t
    | `flag of Flag.t
    | `pp_with_flag of Pp_with_flag.t
    ]
  [@@deriving compare, equal, sexp]
end
