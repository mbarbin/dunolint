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
    ~summary:"A test util to find and print the workspace root."
    ~readme:(fun () ->
      "The root of the current dune workspace is determined by looking up a \
       dune-workspace or dune-project file in the current directory and its parent \
       directories. Dune requires at least one of these two files to operate.\n\n\
       See also $(i,https://dune.readthedocs.io/en/stable/usage.html#finding-the-root).")
    (let open Command.Std in
     let+ () = Log_cli.set_config ()
     and+ root = Common_helpers.root in
     let workspace_root =
       Workspace_root.find_exn ~default_is_cwd:false ~specified_by_user:root
     in
     print_endline (Absolute_path.to_string (Workspace_root.path workspace_root)))
;;
