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

let extended_range ~original_contents ~(range : Loc.Range.t) =
  let len = String.length original_contents in
  let start = range.start in
  let stop =
    let rec loop i =
      if i >= len
      then i
      else (
        match original_contents.[i] with
        | ' ' | '\t' -> loop (i + 1)
        | ';' ->
          (* This is the case in which we'd like to capture the remaining of the
             line. *)
          let rec eol i =
            if i >= len
            then i
            else (
              match original_contents.[i] with
              | '\n' -> i
              | _ -> eol (i + 1))
          in
          eol i
        | _ ->
          (* Keeping the original bound when only looped through spaces and
             tabs. *)
          range.stop)
    in
    loop range.stop
  in
  { Loc.Range.start; stop }
;;

let are_in_different_sections
      ~(previous : Parsexp.Positions.range)
      ~(current : Parsexp.Positions.range)
  =
  let previous_line = previous.end_pos.line in
  let current_line = current.start_pos.line in
  previous_line + 1 < current_line
;;
