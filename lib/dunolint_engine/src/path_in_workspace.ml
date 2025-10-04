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

type t = Relative_path.t

let is_parent_segment s = String.equal ".." s
let has_parent_segments segs = List.exists ~f:is_parent_segment segs

let check_escape_path_exn (t : t) =
  if has_parent_segments (Fpath.segs (t :> Fpath.t))
  then
    invalid_arg
      (Printf.sprintf
         "'%s': relative path escapes upward past starting point"
         (Relative_path.to_string t))
;;

let chop_prefix t ~prefix =
  if Relative_path.equal prefix Relative_path.empty
  then Some t
  else (
    match Relative_path.chop_prefix t ~prefix with
    | None -> None
    | Some t as some ->
      check_escape_path_exn t;
      some)
;;

let parent t =
  if Relative_path.equal t Relative_path.empty
  then None
  else (
    match Relative_path.parent t with
    | None ->
      (* This is the problematic case from upstream, as the function never
         returns [None]. Pending upgrades and TBD. *)
      None
      [@coverage off]
    | Some t as some ->
      check_escape_path_exn t;
      some)
;;

let ancestors_autoloading_dirs ~path =
  if Relative_path.equal path Relative_path.empty
  then []
  else (
    check_escape_path_exn path;
    let segs = Fpath.segs (Relative_path.rem_empty_seg path :> Fpath.t) in
    List.init (List.length segs) ~f:(fun i ->
      List.take segs i
      |> List.map ~f:Fsegment.v
      |> Relative_path.of_list
      |> Relative_path.to_dir_path))
;;

let paths_to_check_for_skip_predicates ~path =
  if Relative_path.equal path Relative_path.empty
  then []
  else (
    check_escape_path_exn path;
    let segs = Fpath.segs (path :> Fpath.t) in
    let ancestors =
      List.init
        (List.length segs - 1)
        ~f:(fun i ->
          List.take segs (i + 1)
          |> List.map ~f:Fsegment.v
          |> Relative_path.of_list
          |> Relative_path.to_dir_path)
    in
    (* For directories, the last ancestor is already the directory itself.
       For files, we need to append the file path. *)
    if List.mem ancestors path ~equal:Relative_path.equal
    then ancestors
    else ancestors @ [ path ])
;;
