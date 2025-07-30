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

  let re_export name =
    Re_export
      { name
      ; source = Printf.sprintf "(re_export %s)" (Dune.Library.Name.to_string name)
      }
  ;;

  let unhandled ~original_index ~sexp =
    Unhandled { original_index; sexp; source = Sexp.to_string_hum sexp }
  ;;

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

  let library_name = function
    | Unhandled { original_index = _; sexp = _; source = _ } -> None
    | Re_export { name; source = _ } -> Some name
    | Library { name; source = _ } -> Some name
  ;;
end

module Section = struct
  type t = { mutable entries : Entry.t list } [@@deriving sexp_of]
end

type t = { mutable sections : Section.t list } [@@deriving sexp_of]

let create ~libraries =
  match List.map libraries ~f:Entry.library with
  | [] -> { sections = [] }
  | _ :: _ as entries -> { sections = [ { entries } ] }
;;

let field_name = "libraries"
let is_empty t = List.for_all t.sections ~f:(fun section -> List.is_empty section.entries)
let entries t = List.concat_map t.sections ~f:(fun section -> section.entries)

let mem t ~library =
  List.exists t.sections ~f:(fun section ->
    List.exists section.entries ~f:(function
      | Unhandled _ -> false
      | Re_export { name; _ } | Library { name; _ } ->
        Dune.Library.Name.equal name library))
;;

let dedup_and_sort t =
  let names = Hash_set.create (module Dune.Library.Name) in
  List.iter t.sections ~f:(fun section ->
    let entries =
      List.dedup_and_sort section.entries ~compare:Entry.For_sort.compare
      |> List.filter ~f:(fun (entry : Entry.t) ->
        match entry with
        | Unhandled _ -> true
        | Re_export { name; _ } | Library { name; _ } ->
          let present = Hash_set.mem names name in
          Hash_set.add names name;
          not present)
    in
    section.entries <- entries)
;;

let add_entries t ~entries =
  let names = Hash_set.create (module Dune.Library.Name) in
  List.iter t.sections ~f:(fun section ->
    List.iter section.entries ~f:(function
      | Unhandled _ -> ()
      | Re_export { name; _ } | Library { name; _ } -> Hash_set.add names name));
  let section =
    match List.last t.sections with
    | Some section -> section
    | None ->
      let section = { Section.entries = [] } in
      t.sections <- [ section ];
      section
  in
  section.entries
  <- section.entries
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

let get_source ~original_contents ~range =
  let { Loc.Range.start; stop } =
    Dunolinter.Comment_handler.extended_range ~original_contents ~range
  in
  String.sub original_contents ~pos:start ~len:(stop - start)
;;

let read ~sexps_rewriter ~field =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let args = Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field in
  let sections =
    List.mapi args ~f:(fun original_index arg ->
      let position = Sexps_rewriter.position sexps_rewriter arg in
      let range = Sexps_rewriter.Position.range position in
      let source = get_source ~original_contents ~range in
      let entry =
        match arg with
        | Atom name -> Entry.Library { name = Dune.Library.Name.v name; source }
        | List [ Atom "re_export"; Atom name ] ->
          Entry.Re_export { name = Dune.Library.Name.v name; source }
        | List _ as sexp -> Entry.Unhandled { original_index; sexp; source }
      in
      position, entry)
    |> List.group ~break:(fun (previous, _) (current, _) ->
      Dunolinter.Comment_handler.are_in_different_sections ~previous ~current)
    |> List.map ~f:(fun entries -> { Section.entries = List.map entries ~f:snd })
  in
  { sections }
;;

let write (t : t) =
  Sexp.List
    (Atom field_name
     :: List.concat_map t.sections ~f:(fun section ->
       List.map section.entries ~f:(function
         (* When producing a new sexp we cannot include the comments because we
            are not in control of the formatting. However there should not be a
            code path that ends up dropping comment, because of a global
            invariant that [write] is only used with [t] values created via this
            interface, and this code path doesn't allow populating comments in
            the first place. *)
         | Library { name; _ } -> Sexp.Atom (Dune.Library.Name.to_string name)
         | Re_export { name; _ } ->
           Sexp.List [ Atom "re_export"; Atom (Dune.Library.Name.to_string name) ]
         | Unhandled { original_index = _; sexp; source = _ } -> sexp)))
;;

let extended_range ~sexps_rewriter ~arg =
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let original_contents = File_rewriter.original_contents file_rewriter in
  let range = Sexps_rewriter.range sexps_rewriter arg in
  Dunolinter.Comment_handler.extended_range ~original_contents ~range
;;

let rewrite t ~sexps_rewriter ~field =
  let args =
    Dunolinter.Sexp_handler.get_args ~field_name ~sexps_rewriter ~field
    |> List.map ~f:(fun arg ->
      let position = Sexps_rewriter.position sexps_rewriter arg in
      position, arg)
    |> List.group ~break:(fun (previous, _) (current, _) ->
      Dunolinter.Comment_handler.are_in_different_sections ~previous ~current)
    |> List.map ~f:(List.map ~f:snd)
  in
  let file_rewriter = Sexps_rewriter.file_rewriter sexps_rewriter in
  let insert_position =
    let last_token =
      match (field : Sexp.t) with
      | List token_list -> List.last_exn token_list
      | Atom _ -> assert false
    in
    (extended_range ~sexps_rewriter ~arg:last_token).stop
  in
  let write_arg = function
    | Entry.Library { name = _; source } -> source
    | Entry.Re_export { name = _; source } -> source
    | Entry.Unhandled { original_index = _; sexp = _; source } -> source
  in
  let rec iter_fields args new_args =
    match args, new_args with
    | arg :: args, new_arg :: new_args ->
      File_rewriter.replace
        file_rewriter
        ~range:(extended_range ~sexps_rewriter ~arg)
        ~text:(write_arg new_arg);
      iter_fields args new_args
    | [], [] -> ()
    | [], _ :: _ ->
      List.iter new_args ~f:(fun new_arg ->
        let value = write_arg new_arg in
        File_rewriter.insert file_rewriter ~offset:insert_position ~text:("\n" ^ value))
    | _ :: _, [] ->
      List.iter args ~f:(fun arg ->
        File_rewriter.remove file_rewriter ~range:(extended_range ~sexps_rewriter ~arg))
  in
  let rec iter_sections args new_args =
    match args, new_args with
    | [], [] -> ()
    | args :: tl, new_args :: new_tl ->
      iter_fields args new_args.Section.entries;
      iter_sections tl new_tl
    | [], new_args ->
      List.iter new_args ~f:(fun new_args -> iter_fields [] new_args.Section.entries)
    | args, [] -> List.iter args ~f:(fun args -> iter_fields args [])
  in
  iter_sections args t.sections
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

module Private = struct
  module Entry = Entry
end
