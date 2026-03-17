(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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

let enforce =
  Dunolinter.Linter.enforce
    (module Nothing)
    ~eval
    ~enforce:(fun _ predicate ->
      match[@coverage off] predicate with
      | T x | Not x -> Nothing.unreachable_code x)
;;
