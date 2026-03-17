(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
