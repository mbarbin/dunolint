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

let ancestors_autoloading_dirs ~path =
  if Relative_path.equal path Relative_path.empty
  then []
  else (
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
