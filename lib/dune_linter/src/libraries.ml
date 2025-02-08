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

module Entry = struct
  type t =
    | Unhandled of
        { original_index : int
        ; sexp : Sexp.t
        ; source : string
        }
    | Re_export of
        { name : Dune.Library.Name.t
        ; source : string
        }
    | Library of
        { name : Dune.Library.Name.t
        ; source : string
        }
  [@@deriving sexp_of]

  let library name = Library { name; source = Dune.Library.Name.to_string name }

  module For_sort = struct
    let compare t1 t2 =
      match t1, t2 with
      | Unhandled { original_index = i1; _ }, Unhandled { original_index = i2; _ } ->
        Int.compare i1 i2
      | Unhandled _, (Library _ | Re_export _) -> 1
      | (Library _ | Re_export _), Unhandled _ -> -1
      | ( (Library { name = n1; _ } | Re_export { name = n1; _ })
        , (Library { name = n2; _ } | Re_export { name = n2; _ }) ) ->
        Dune.Library.Name.compare n1 n2
    ;;
  end
end

type t = { mutable entries : Entry.t list } [@@deriving sexp_of]

let create ~libraries =
  let entries = List.map libraries ~f:Entry.library in
  { entries }
;;

let field_name = "libraries"
let is_empty t = List.is_empty t.entries
let entries t = t.entries

let mem t ~library =
  List.exists t.entries ~f:(function
    | Unhandled _ -> false
    | Re_export { name; _ } | Library { name; _ } -> Dune.Library.Name.equal name library)
;;

let dedup_and_sort t =
  t.entries <- List.dedup_and_sort t.entries ~compare:Entry.For_sort.compare
;;

let add_entries t ~entries =
  let names = Hash_set.create (module Dune.Library.Name) in
  List.iter t.entries ~f:(function
    | Unhandled _ -> ()
    | Re_export { name; _ } | Library { name; _ } -> Hash_set.add names name);
  t.entries
  <- t.entries
     @ List.filter_map entries ~f:(fun entry ->
       match (entry : Entry.t) with
       | Unhandled _ -> None
       | Re_export { name; _ } | Library { name; _ } ->
         if Hash_set.mem names name
         then None
         else (
           Hash_set.add names name;
           Some entry))
;;

let add_libraries t ~libraries =
  add_entries t ~entries:(List.map libraries ~f:Entry.library)
;;

let get_range ~sexps_rewriter ~field =
  let range = Sexps_rewriter.range sexps_rewriter field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let len = String.length original_contents in
  let start = range.start in
  (* With this we handle a very particular but common case of comments
     fitting in one line indicated at the right of the value. We
     included the comment in the source in this case. *)
  let stop =
    let rec loop i =
      if i >= len
      then i
      else (
        match original_contents.[i] with
        | ' ' | '\t' -> loop (i + 1)
        | ';' ->
          (* This is the case in which we'd like to capture the
             remaining of the line. *)
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
          (* Keeping the original bound when only looped through
             spaces and tabs. *)
          range.stop)
    in
    loop range.stop
  in
  { Loc.Range.start; stop }
;;

let get_source ~sexps_rewriter ~field =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let { Loc.Range.start; stop } = get_range ~sexps_rewriter ~field in
  String.sub original_contents ~pos:start ~len:(stop - start)
;;

let read ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let entries =
    List.mapi args ~f:(fun original_index arg ->
      match arg with
      | Atom name ->
        let source = get_source ~sexps_rewriter ~field:arg in
        Entry.Library { name = Dune.Library.Name.v name; source }
      | List [ Atom "re_export"; Atom name ] ->
        let source = get_source ~sexps_rewriter ~field:arg in
        Entry.Re_export { name = Dune.Library.Name.v name; source }
      | List _ as sexp ->
        let source = get_source ~sexps_rewriter ~field:arg in
        Entry.Unhandled { original_index; sexp; source })
  in
  { entries }
;;

let write (t : t) =
  Sexp.List
    (Atom field_name
     :: List.map t.entries ~f:(function
       (* When producing a new sexp we cannot include the comments
          because we are not in control of the formatting. However
          there should not be a code path that ends up dropping
          comment, because of a global invariant that [write] is
          only used with [t] created via this interface in the first
          place, and this doesn't allow populating comments. *)
       | Library { name; _ } -> Sexp.Atom (Dune.Library.Name.to_string name)
       | Re_export { name; _ } ->
         Sexp.List [ Atom "re_export"; Atom (Dune.Library.Name.to_string name) ]
       | Unhandled { original_index = _; sexp; source = _ } -> sexp))
;;

let rewrite t ~sexps_rewriter ~field =
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let last_offset =
    match List.last args with
    | None -> Loc.stop_offset (Sexps_rewriter.loc sexps_rewriter field)
    | Some arg -> (get_range ~sexps_rewriter ~field:arg).stop
  in
  let write_arg = function
    | Entry.Library { name = _; source } -> source
    | Entry.Re_export { name = _; source } -> source
    | Entry.Unhandled { original_index = _; sexp = _; source } -> source
  in
  let new_args = t.entries in
  let rec iter_fields args new_args =
    match args, new_args with
    | arg :: args, new_arg :: new_args ->
      File_rewriter.replace
        file_rewriter
        ~range:(get_range ~sexps_rewriter ~field:arg)
        ~text:(write_arg new_arg);
      iter_fields args new_args
    | [], [] -> ()
    | [], _ :: _ ->
      List.iter new_args ~f:(fun new_arg ->
        let value = write_arg new_arg in
        File_rewriter.insert file_rewriter ~offset:last_offset ~text:("\n" ^ value))
    | _ :: _, [] ->
      List.iter args ~f:(fun arg ->
        File_rewriter.remove file_rewriter ~range:(get_range ~sexps_rewriter ~field:arg))
  in
  iter_fields args new_args
;;

type predicate = Nothing.t

let eval _t ~predicate =
  match (predicate : predicate) with
  | x -> Nothing.unreachable_code x
;;

let rec enforce t ~condition =
  match (condition : predicate Blang.t) with
  | Base x -> Nothing.unreachable_code x
  | (And _ | If _ | True | False | Not _ | Or _) as condition ->
    Dunolinter.Linter.enforce_blang (module Nothing) t ~condition ~eval ~enforce
;;
