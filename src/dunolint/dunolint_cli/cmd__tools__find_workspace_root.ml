(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
