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

type 'a linter =
  { field_name : string
  ; linter : 'a
  }

type 'a t = 'a linter array

let create linters ~field_name =
  linters
  |> List.map ~f:(fun linter -> { field_name = field_name linter; linter })
  |> List.sort ~compare:(Comparable.lift String.compare ~f:(fun t -> t.field_name))
  |> Array.of_list
;;

let lookup linters ~field_name =
  Binary_search.binary_search
    linters
    ~length:Array.length
    ~get:(fun t i -> t.(i).field_name)
    ~compare:String.compare
    `First_equal_to
    field_name
  |> Option.map ~f:(fun index -> linters.(index).linter)
;;
