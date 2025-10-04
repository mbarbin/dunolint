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
    ~summary:"Lint project."
    ~readme:(fun () ->
      "This command lints files in a dune project starting from the workspace root.\n\n\
       Dunolint will first locate the workspace root by searching for \
       $(b,dune-workspace) or $(b,dune-project) files in the current directory and its \
       ancestors, then change to that directory before performing linting operations.\n\n\
       The workspace root can be overridden using the $(b,--root) flag. If a \
       $(b,dunolint) config file exists at the workspace root, it will be loaded \
       automatically unless $(b,--config) is specified.\n\n\
       Use $(b,--below) to limit linting to a specific subdirectory while still using \
       the workspace root's configuration.")
    (let open Command.Std in
     let+ running_mode = Dunolint_engine.Running_mode.arg
     and+ () = Log_cli.set_config ()
     and+ config =
       Arg.named_opt [ "config" ] Param.file ~doc:"Path to dunolint config file."
     and+ below = Common_helpers.below ~doc:"Lint only below this path."
     and+ enforce =
       Arg.named_multi
         [ "enforce" ]
         (Common_helpers.sexpable_param (module Dunolint.Condition))
         ~docv:"COND"
         ~doc:"Add condition to enforce."
       >>| List.map ~f:(fun condition -> `enforce condition)
     and+ root = Common_helpers.root in
     let cwd = Unix.getcwd () |> Absolute_path.v in
     let workspace_root =
       Workspace_root.find_exn ~default_is_cwd:false ~specified_by_user:root
     in
     let below =
       Option.map below ~f:(fun below ->
         Common_helpers.relativize ~workspace_root ~cwd ~path:below)
     in
     let config =
       Option.map config ~f:(fun config ->
         Common_helpers.relativize ~workspace_root ~cwd ~path:(Fpath.v config)
         |> Relative_path.to_string)
     in
     Workspace_root.chdir workspace_root ~level:Warning;
     let root_configs =
       [ Common_helpers.load_config_opt_exn ~config ~append_extra_rules:enforce ]
     in
     Dunolint_engine.run ~root_configs ~running_mode
     @@ fun dunolint_engine ->
     Dunolint_engine.visit
       dunolint_engine
       ?below
       ~f:(fun ~context ~parent_dir ~subdirectories:_ ~files ->
         Linter.visit_directory ~dunolint_engine ~context ~parent_dir ~files))
;;
