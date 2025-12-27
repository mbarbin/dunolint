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

module type S = Sexp_handler_intf.S

let replace_field ~sexps_rewriter ~field ~new_field =
  if not ([%equal: Sexp.t] field new_field)
  then (
    let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
    File_rewriter.replace
      file_rewriter
      ~range:(Sexps_rewriter.range sexps_rewriter field)
      ~text:(Sexp.to_string_hum new_field))
;;

let find (type a) (module M : S with type t = a) ~sexps_rewriter ~fields =
  List.find_map fields ~f:(fun field ->
    match (field : Sexp.t) with
    | List (Atom name :: _) when String.equal name M.field_name ->
      Some (M.read ~sexps_rewriter ~field)
    | _ -> None)
;;

let get_args ~field_name ~sexps_rewriter ~field =
  match field with
  | Sexp.List (Atom field_name' :: args) when String.equal field_name field_name' -> args
  | _ ->
    Err.raise
      ~loc:(Sexps_rewriter.loc sexps_rewriter field)
      Pp.O.
        [ Pp.text "Unexpected "
          ++ Pp_tty.kwd (module String) field_name
          ++ Pp.text " field."
        ]
;;

module Make_sexpable
    (M : sig
       val field_name : string
     end)
    (S : Sexpable.S) =
struct
  type t = S.t

  let field_name = M.field_name

  let read ~sexps_rewriter ~field =
    match field with
    | Sexp.List [ Atom field_name; value ] when String.equal field_name M.field_name ->
      S.t_of_sexp value
    | _ ->
      Err.raise
        ~loc:(Sexps_rewriter.loc sexps_rewriter field)
        Pp.O.
          [ Pp.textf "Unexpected [Sexp] for field "
            ++ Pp_tty.kwd (module String) M.field_name
            ++ Pp.text "."
          ]
  ;;

  let write (t : t) = Sexp.List [ Atom M.field_name; S.sexp_of_t t ]

  let rewrite (t : t) ~sexps_rewriter ~field =
    replace_field ~sexps_rewriter ~field ~new_field:(write t)
  ;;
end

module Make_atom (M : sig
    val field_name : string
  end) =
  Make_sexpable (M) (String)

module Make_sexp_list (M : sig
    val field_name : string
  end) =
struct
  type t = Sexp.t list

  let field_name = M.field_name

  let read ~sexps_rewriter ~field =
    get_args ~field_name:M.field_name ~sexps_rewriter ~field
  ;;

  let write (t : t) = Sexp.List (Atom M.field_name :: t)

  let rewrite (t : t) ~sexps_rewriter ~field =
    replace_field ~sexps_rewriter ~field ~new_field:(write t)
  ;;
end

module Make_sexpable_list
    (M : sig
       val field_name : string
     end)
    (S : Sexpable.S) =
struct
  type t = S.t list

  let field_name = M.field_name

  let read ~sexps_rewriter ~field =
    get_args ~field_name:M.field_name ~sexps_rewriter ~field |> List.map ~f:S.t_of_sexp
  ;;

  let write (t : t) = Sexp.List (Atom M.field_name :: List.map t ~f:S.sexp_of_t)

  let rewrite (t : t) ~sexps_rewriter ~field =
    replace_field ~sexps_rewriter ~field ~new_field:(write t)
  ;;
end

module Make_sexpable_ordered_set
    (M : sig
       val field_name : string
     end)
    (S : Sexpable.S) =
struct
  type t = S.t Ordered_set.t

  let field_name = M.field_name
  let read_element ~sexps_rewriter:_ sexp = S.t_of_sexp sexp

  let read ~sexps_rewriter ~field =
    let args = get_args ~field_name:M.field_name ~sexps_rewriter ~field in
    Ordered_set.read ~read_element ~sexps_rewriter args
  ;;

  let write (t : t) =
    let values = Ordered_set.write ~write_a:S.sexp_of_t t in
    Sexp.List (Atom M.field_name :: values)
  ;;

  let rewrite (t : t) ~sexps_rewriter ~field =
    replace_field ~sexps_rewriter ~field ~new_field:(write t)
  ;;
end

let insert_new_fields ~sexps_rewriter ~indicative_field_ordering ~fields ~new_fields =
  let new_fields =
    List.map new_fields ~f:(fun (field : Sexp.t) ->
      let name =
        match field with
        | List (Atom name :: _) -> name
        | _ ->
          Err.raise
            [ Pp.text "Unexpected field shape"; Pp.text (Sexp.to_string_hum field) ]
          [@coverage off]
      in
      ref false, name, field)
  in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  (* We insert all missing fields. *)
  List.iter fields ~f:(fun field ->
    match (field : Sexp.t) with
    | List (Atom field :: _) ->
      List.iter new_fields ~f:(fun (visited, field_name, _) ->
        if String.equal field field_name then visited := true)
    | _ -> ());
  List.iter new_fields ~f:(fun (visited, field_name, new_field) ->
    if not !visited
    then (
      (* To compute the place of insertion we skip input fields as long as they
         appear prior to this field. When we can no longer, we insert after the
         last one. *)
      let field_names_located_before =
        let rec aux acc = function
          | [] -> acc
          | hd :: tl ->
            if String.equal hd field_name then acc else aux (Set.add acc hd) tl
        in
        aux (Set.empty (module String)) indicative_field_ordering
      in
      let pred_field =
        let rec aux last = function
          | [] -> last
          | hd :: tl ->
            (match (hd : Sexp.t) with
             | List (Atom name :: _) ->
               if Set.mem field_names_located_before name then aux hd tl else last
             | _ -> last)
        in
        match fields with
        | hd :: tl -> aux hd tl
        | [] ->
          Err.raise
            ~loc:(Loc.of_file ~path:(Sexps_rewriter.path sexps_rewriter))
            [ Pp.textf "Existing stanza in dune file expected to have at least one field."
            ] [@coverage off]
      in
      let pred_loc = Sexps_rewriter.loc sexps_rewriter pred_field in
      let indentation =
        (* The intention here is to help when auto fmt is not
           available for that dune file. *)
        let pred_pos = Loc.start pred_loc in
        let pred_indent = pred_pos.pos_cnum - pred_pos.pos_bol in
        String.make pred_indent ' '
      in
      File_rewriter.insert
        file_rewriter
        ~offset:(Loc.stop_offset pred_loc)
        ~text:("\n" ^ indentation ^ Sexp.to_string_hum new_field)))
;;

let loc_of_parsexp_range ~filename (range : Parsexp.Positions.range) =
  let source_code_position ({ line; col; offset } : Parsexp.Positions.pos) =
    { Lexing.pos_fname = filename
    ; pos_lnum = line
    ; pos_cnum = offset
    ; pos_bol = offset - col
    }
  in
  Loc.create (source_code_position range.start_pos, source_code_position range.end_pos)
;;

module Error_context = Dunolint.Private.Sexp_helpers.Error_context

let render_sexp_error_exn ~loc exn =
  match exn with
  | Err.E err -> err
  | Failure str ->
    let message = Pp.text (if String.is_suffix str ~suffix:"." then str else str ^ ".") in
    Err.create ~loc [ message ]
  | Error_context.E context ->
    let hints =
      List.concat
        [ (match Error_context.did_you_mean context with
           | None -> []
           | Some { var; candidates } -> Err.did_you_mean var ~candidates)
        ; (match Error_context.suggestion context with
           | None -> []
           | Some text -> [ Pp.text text ])
        ]
    in
    Err.create ~loc [ Pp.text (Error_context.message context) ] ~hints
  | exn -> Err.create ~loc [ Err.exn exn ] [@coverage off]
;;

let read (type a) (module M : S with type t = a) ~sexps_rewriter ~field =
  match M.read ~sexps_rewriter ~field with
  | ok -> Ok ok
  | exception Err.E err -> Error err
  | exception Sexp.Of_sexp_error (exn, sexp) ->
    let loc = Sexps_rewriter.loc sexps_rewriter sexp in
    Error (render_sexp_error_exn ~loc exn)
;;
