(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* An item is an element of a list along with the comment placed after it on the
   same line, if any. [stop] is the end of that comment. *)
module Item = struct
  type t =
    { sexp : Sexp.t
    ; position : Parsexp.Positions.range
    ; start : int
    ; stop : int
    ; has_comment : bool
    ; text : string
    }
end

(* Returns [gap] without its leading blanks if they're not followed by a newline, that
   is when [gap] continues on the same line as the item it follows. *)
let same_line_rest gap =
  let len = String.length gap in
  let rec loop i =
    if i >= len
    then Some ""
    else (
      match gap.[i] with
      | ' ' | '\t' -> loop (i + 1)
      | '\n' -> None
      | _ -> Some (String.sub gap ~pos:i ~len:(len - i)))
  in
  loop 0
;;

let reorder ~sexps_rewriter ~sexp ~recurse_into ~is_fixed ~compare =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let sub ~start ~stop = String.sub original_contents ~pos:start ~len:(stop - start) in
  let rec render (sexp : Sexp.t) =
    let range = Sexps_rewriter.range sexps_rewriter sexp in
    match sexp with
    | Atom _ | List [] -> sub ~start:range.start ~stop:range.stop
    | List (_ :: _ as elements) when not (recurse_into elements) ->
      sub ~start:range.start ~stop:range.stop
    | List (_ :: _ as elements) ->
      let items =
        List.map elements ~f:(fun sexp ->
          let position = Sexps_rewriter.position sexps_rewriter sexp in
          let range = Sexps_rewriter.Position.range position in
          let extended = Comment_handler.extended_range ~original_contents ~range in
          { Item.sexp
          ; position
          ; start = range.start
          ; stop = extended.stop
          ; has_comment = extended.stop > range.stop
          ; text = render sexp ^ sub ~start:range.stop ~stop:extended.stop
          })
      in
      (* The items are reordered within runs of items that are neither fixed nor
         separated by a section boundary. *)
      let ordered =
        items
        |> List.mapi ~f:(fun i (item : Item.t) ->
          is_fixed ~is_head:(Int.equal i 0) item.sexp, item)
        |> List.group
             ~break:
               (fun
                 (previous_is_fixed, (previous : Item.t)) (current_is_fixed, current) ->
               previous_is_fixed
               || current_is_fixed
               || Sections_handler.are_in_different_sections
                    ~previous:previous.position
                    ~current:current.position)
        |> List.concat_map ~f:(fun run ->
          List.sort (List.map run ~f:snd) ~compare:(fun (a : Item.t) (b : Item.t) ->
            Ordering.to_int (compare a.sexp b.sexp)))
      in
      (* The gaps between the items, starting with the one before the first item, and
         ending with the one after the last item. *)
      let gaps =
        let stops = range.start :: List.map items ~f:(fun (item : Item.t) -> item.stop) in
        let starts =
          List.map items ~f:(fun (item : Item.t) -> item.start) @ [ range.stop ]
        in
        List.map2 stops starts ~f:(fun start stop -> sub ~start ~stop)
      in
      (* The column of the first element, which is also the one of the elements placed
         on their own line in formatted files. *)
      let indentation = (List.hd items : Item.t).position.start_pos.col in
      let buffer = Buffer.create (range.stop - range.start) in
      Buffer.add_string buffer (List.hd gaps);
      List.iter2 ordered (List.tl gaps) ~f:(fun (item : Item.t) gap ->
        Buffer.add_string buffer item.text;
        match if item.has_comment then same_line_rest gap else None with
        | None -> Buffer.add_string buffer gap
        | Some rest ->
          (* The comment of the item would hide what follows on the same line. *)
          Buffer.add_char buffer '\n';
          Buffer.add_string buffer (String.make indentation ' ');
          Buffer.add_string buffer rest);
      Buffer.contents buffer
  in
  render sexp
;;
