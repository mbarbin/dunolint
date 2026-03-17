(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

let sexp_extended_range ~sexps_rewriter ~arg =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let range = Sexps_rewriter.range sexps_rewriter arg in
  extended_range ~original_contents ~range
;;

let get_extended_source ~original_contents ~range =
  let { Loc.Range.start; stop } = extended_range ~original_contents ~range in
  String.sub original_contents ~pos:start ~len:(stop - start)
;;
