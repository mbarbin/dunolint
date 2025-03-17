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

type t = { mutable flags : Sexp.t list } [@@deriving sexp_of]

let field_name = "flags"
let create ~flags = { flags }
let is_empty { flags } = List.is_empty flags
let flags t = t.flags
let set_flags t ~flags = t.flags <- flags

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  { flags = args }
;;

let write (t : t) = Sexp.List (Atom field_name :: t.flags)

let rewrite (t : t) ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let insert_position =
    let last_token =
      match (field : Sexp.t) with
      | List token_list -> List.last_exn token_list
      | Atom _ -> assert false
    in
    Sexps_rewriter.stop_offset sexps_rewriter last_token
  in
  let rec iter expected_exprs actual_exprs =
    match expected_exprs, actual_exprs with
    | [], _ -> ()
    | rest, [] ->
      let rest = List.map rest ~f:Sexp.to_string_hum in
      File_rewriter.insert
        file_rewriter
        ~offset:insert_position
        ~text:(" " ^ String.concat rest ~sep:" ")
    | expected_expr :: expected_exprs, (actual_expr :: actual_exprs as all_actual_exprs)
      ->
      if Sexp.equal expected_expr actual_expr
      then iter expected_exprs actual_exprs
      else (
        let do_insert =
          match expected_exprs with
          | [] -> false
          | next_expr :: _ -> Sexp.equal next_expr actual_expr
        in
        if do_insert
        then (
          File_rewriter.insert
            file_rewriter
            ~offset:(Sexps_rewriter.start_offset sexps_rewriter actual_expr)
            ~text:(Sexp.to_string_hum expected_expr ^ " ");
          iter expected_exprs all_actual_exprs)
        else (
          File_rewriter.replace
            file_rewriter
            ~range:(Sexps_rewriter.range sexps_rewriter actual_expr)
            ~text:(Sexp.to_string_hum expected_expr);
          iter expected_exprs actual_exprs))
  in
  iter t.flags args
;;

type predicate = Nothing.t

let eval _t ~predicate =
  match[@coverage off] (predicate : predicate) with
  | x -> Nothing.unreachable_code x
;;

let rec enforce t ~condition =
  match (condition : predicate Blang.t) with
  | Base x -> Nothing.unreachable_code x [@coverage off]
  | (And _ | If _ | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang
      (module Nothing)
      t
      ~condition
      ~eval
      ~enforce [@coverage off]
  | (True | False) as condition ->
    Dunolinter.Linter.enforce_blang (module Nothing) t ~condition ~eval ~enforce
;;
