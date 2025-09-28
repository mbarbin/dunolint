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

(* This module is derived from the dune code base file
   * [./bin/workspace_root.mli] which is released under MIT:
   *
   * Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE.
   *
   *  Changes:
   *
   * - Remove explicit dependency to [Stdune] - use [Base] instead.
   * - Inline source file names (e.g. "dune-workspace").
   * - Use [Err] instead of [User_message].
   * - Rename [create_exn] as [find_exn].
   * - Use polymorphic variant and subtypes for [Kind].
   * - Remove [reach_from_root_prefix].
*)

module Source_filename = struct
  let dune_project = "dune-project"
  let dune_workspace = "dune-workspace"
end

type t = Absolute_path.t

let path t = t

module Candidate_kind = struct
  type t =
    [ `Dune_workspace
    | `Dune_project
    ]

  let priority = function
    | `Dune_workspace -> 1
    | `Dune_project -> 2
  ;;

  let lowest_priority = Int.max_value

  let of_dir_contents files =
    if Set.mem files Source_filename.dune_workspace
    then Some `Dune_workspace
    else if Set.mem files Source_filename.dune_project
    then Some `Dune_project
    else None
  ;;
end

module Candidate = struct
  type t =
    { dir : string
    ; to_cwd : string list
    ; kind : Candidate_kind.t
    }
end

let find () =
  let cwd = Unix.getcwd () in
  let rec loop counter ~(candidate : Candidate.t option) ~to_cwd dir : Candidate.t option =
    match Stdlib.Sys.readdir dir with
    | exception Sys_error msg ->
      (Err.warning
         [ Pp.textf
             "Unable to read directory %s. Will not look for root in parent directories."
             dir
         ; Pp.textf "Reason: %s" msg
         ; Pp.text "To remove this warning, set your root explicitly using --root."
         ];
       candidate)
      [@coverage off]
    | files ->
      let files = Set.of_list (module String) (Array.to_list files) in
      let candidate =
        let candidate_priority =
          match candidate with
          | Some c -> Candidate_kind.priority c.kind
          | None -> Candidate_kind.lowest_priority
        in
        match Candidate_kind.of_dir_contents files with
        | Some kind when Candidate_kind.priority kind <= candidate_priority ->
          Some { Candidate.kind; dir; to_cwd }
        | _ -> candidate
      in
      cont counter ~candidate dir ~to_cwd
  and cont counter ~candidate ~to_cwd dir =
    if counter > String.length cwd
    then candidate [@coverage off]
    else (
      let parent = Stdlib.Filename.dirname dir in
      if String.equal parent dir
      then candidate
      else (
        let base = Stdlib.Filename.basename dir in
        loop (counter + 1) parent ~candidate ~to_cwd:(base :: to_cwd)))
  in
  loop 0 ~to_cwd:[] cwd ~candidate:None
;;

let find_exn ~default_is_cwd ~specified_by_user =
  match specified_by_user with
  | Some path -> path
  | None ->
    (match find () with
     | Some { dir; to_cwd = _; kind = _ } -> Absolute_path.v dir
     | None ->
       if default_is_cwd
       then Unix.getcwd () |> Absolute_path.v
       else
         Err.raise
           [ Pp.text "I cannot find the root of the current dune workspace/project."
           ; Pp.text "If you would like to create a new dune project, you can type:"
           ; Pp.nop
           ; Pp.verbatim "    dune init project NAME"
           ; Pp.nop
           ; Pp.text
               "Otherwise, please make sure to run dune inside an existing project or \
                workspace. For more information about how dune identifies the root of \
                the current workspace/project, please refer to \
                https://dune.readthedocs.io/en/stable/usage.html#finding-the-root"
           ])
;;
