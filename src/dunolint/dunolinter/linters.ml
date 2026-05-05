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
  let compare_fields t1 t2 = String.compare t1.field_name t2.field_name in
  linters
  |> List.map ~f:(fun linter -> { field_name = field_name linter; linter })
  |> List.sort ~compare:compare_fields
  |> Array.of_list
;;

let lookup linters ~field_name =
  let rec loop left right =
    if left > right
    then None
    else (
      let mid = (left + right) / 2 in
      let entry = linters.(mid) in
      match String.compare field_name entry.field_name |> Ordering.of_int with
      | Eq -> Some entry.linter
      | Lt -> loop left (mid - 1)
      | Gt -> loop (mid + 1) right)
  in
  loop 0 (Array.length linters - 1)
;;
