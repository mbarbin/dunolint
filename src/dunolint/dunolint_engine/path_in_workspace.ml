(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = Relative_path.t

let ancestors_autoloading_dirs ~path =
  if Relative_path.equal path Relative_path.empty
  then []
  else (
    let segs = Fpath.segs (Relative_path.rem_empty_seg path :> Fpath.t) in
    List.init ~len:(List.length segs) ~f:(fun i ->
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
        ~len:(List.length segs - 1)
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
