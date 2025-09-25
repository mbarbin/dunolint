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

let main =
  Command.make
    ~summary:"A util to get info about the enclosing repo."
    ~readme:(fun () ->
      "This command locates the root of the repository containing the current working \
       directory.\n\n\
       It then displays a S-expression containing several fields related to that \
       repository and the current path.\n\n\
       - $(b,repo_root) : The root of the enclosing repo (absolute path).\n\n\
       - $(b,path_in_repo) : The path of the current directory related to the repo root \
       (relative path).\n\n\
       - $(b,vcs_kind) : The kind of version control for the enclosing repository \
       (git|hg).")
    (let open Command.Std in
     let+ below = Common_helpers.below ~doc:"Select a subdirectory." in
     let cwd = Unix.getcwd () |> Absolute_path.v in
     let { Enclosing_repo.vcs_kind; repo_root; vcs = _ } =
       Common_helpers.find_enclosing_repo_exn ~from:cwd
     in
     let path_in_repo =
       Common_helpers.relativize
         ~repo_root
         ~cwd
         ~path:(Option.value below ~default:Relative_path.empty :> Fpath.t)
     in
     print_s
       [%sexp
         { repo_root : Vcs.Repo_root.t
         ; path_in_repo : Vcs.Path_in_repo.t
         ; vcs_kind : Enclosing_repo.Vcs_kind.t
         }];
     ())
;;
