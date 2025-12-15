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

let are_in_different_sections
      ~(previous : Parsexp.Positions.range)
      ~(current : Parsexp.Positions.range)
  =
  let previous_line = previous.end_pos.line in
  let current_line = current.start_pos.line in
  previous_line + 1 < current_line
;;

let read_sections_fold ~field_name ~sexps_rewriter ~field ~init ~f =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let args = Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let _, args =
    List.fold_mapi args ~init ~f:(fun original_index acc arg ->
      let position = Sexps_rewriter.position sexps_rewriter arg in
      let loc = Sexps_rewriter.Position.loc sexps_rewriter position in
      let range = Sexps_rewriter.Position.range position in
      let source = Comment_handler.get_extended_source ~original_contents ~range in
      let acc, entry = f acc ~original_index ~loc ~source ~arg in
      acc, (position, entry))
  in
  args
  |> List.group ~break:(fun (previous, _) (current, _) ->
    are_in_different_sections ~previous ~current)
  |> List.map ~f:(fun entries -> List.map entries ~f:snd)
;;

let read_sections ~field_name ~sexps_rewriter ~field ~f =
  read_sections_fold
    ~field_name
    ~sexps_rewriter
    ~field
    ~init:()
    ~f:(fun () ~original_index ~loc ~source ~arg ->
      (), f ~original_index ~loc ~source ~arg)
;;

let rewrite_sections ~field_name ~sexps_rewriter ~field ~write_arg ~sections =
  let args =
    Sexp_handler.get_args ~field_name ~sexps_rewriter ~field
    |> List.map ~f:(fun arg ->
      let position = Sexps_rewriter.position sexps_rewriter arg in
      position, arg)
    |> List.group ~break:(fun (previous, _) (current, _) ->
      are_in_different_sections ~previous ~current)
    |> List.map ~f:(List.map ~f:snd)
  in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let insert_position =
    let last_token =
      match (field : Sexp.t) with
      | List token_list -> List.last_exn token_list
      | Atom _ -> assert false
    in
    (Comment_handler.sexp_extended_range ~sexps_rewriter ~arg:last_token).stop
  in
  let rec iter_fields args new_args =
    match args, new_args with
    | arg :: args, new_arg :: new_args ->
      File_rewriter.replace
        file_rewriter
        ~range:(Comment_handler.sexp_extended_range ~sexps_rewriter ~arg)
        ~text:(write_arg new_arg);
      iter_fields args new_args
    | [], [] -> ()
    | [], _ :: _ ->
      List.iter new_args ~f:(fun new_arg ->
        let value = write_arg new_arg in
        File_rewriter.insert file_rewriter ~offset:insert_position ~text:("\n" ^ value))
    | _ :: _, [] ->
      List.iter args ~f:(fun arg ->
        File_rewriter.remove
          file_rewriter
          ~range:(Comment_handler.sexp_extended_range ~sexps_rewriter ~arg))
  in
  let rec iter_sections args new_args =
    match args, new_args with
    | [], [] -> ()
    | args :: tl, new_args :: new_tl ->
      iter_fields args new_args;
      iter_sections tl new_tl
    | [], new_args -> List.iter new_args ~f:(fun new_args -> iter_fields [] new_args)
    | args, [] -> List.iter args ~f:(fun args -> iter_fields args [])
  in
  iter_sections args sections
;;
