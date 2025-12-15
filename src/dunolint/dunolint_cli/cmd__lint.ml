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
       ancestors (using the same logic as Dune), then change to that directory before \
       performing linting operations. The workspace root can be overridden using the \
       $(b,--root) flag.\n\n\
       $(b,Config Autoloading:) By default, dunolint will automatically discover and \
       load $(b,dunolint) config files found in the workspace root and any \
       subdirectories during traversal. Configs are accumulated from root down to each \
       linted file's directory.\n\n\
       $(b,Config Accumulation and Precedence:) When multiple configs are loaded (e.g., \
       from root and subdirectories), $(b,all) rules from $(b,all) configs are applied \
       in sequence. Rules from configs deeper in the tree are applied last and take \
       precedence when modifying the same fields.\n\n\
       $(b,Config Autoloading and Manual Override:) Config autoloading is $(b,disabled) \
       when either $(b,--config) or $(b,--enforce) flags are supplied. The $(b,--config) \
       flag specifies an explicit config file to use instead of autoloading. The \
       $(b,--enforce) flag adds specific conditions to enforce. Both flags can be used \
       together, and both treat paths as if resolved from the workspace root. Note that \
       default skip paths are $(b,always) applied regardless of which flags are used.\n\n\
       Use $(b,--below) to limit linting to a specific subdirectory. When using \
       $(b,--below), configs from ancestor directories (including the workspace root) \
       are still loaded and applied.")
    (let open Command.Std in
     let+ running_mode = Dunolint_engine.Running_mode.arg
     and+ () = Log_cli.set_config ()
     and+ config =
       Arg.named_opt
         [ "config" ]
         Param.file
         ~doc:
           "Path to dunolint config file. When specified, config autoloading is disabled \
            and only this config is used. The config is evaluated as if it were located \
            at the workspace root (important for path resolution). This flag is \
            primarily meant for backward compatibility and quick testing; new code and \
            persistent setups should use dunolint config files directly."
     and+ below = Common_helpers.below ~doc:"Lint only below this path."
     and+ enforce =
       Arg.named_multi
         [ "enforce" ]
         (Common_helpers.sexpable_param (module Dunolint.Condition))
         ~docv:"COND"
         ~doc:
           "Add condition to enforce. Can be specified multiple times. $(b,Deprecated): \
            This flag is primarily for backward compatibility and testing. For \
            persistent rules, add them to a dunolint config file. For one-off \
            transitions and quick edits, future dedicated tooling is planned. This flag \
            can be used alone or combined with $(b,--config). When combined, the config \
            file is applied first, then enforce rules are applied last."
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
       List.concat
         [ [ Common_helpers.default_skip_paths_config () ]
         ; (match config with
            | None -> []
            | Some config_path ->
              [ Dunolinter.Config_handler.load_config_exn ~filename:config_path ])
         ; (match Common_helpers.enforce_rules_config ~rules:enforce with
            | None -> []
            | Some config -> [ config ])
         ]
     in
     Dunolint_engine.run ~root_configs ~running_mode
     @@ fun dunolint_engine ->
     Dunolint_engine.visit
       dunolint_engine
       ~autoload_config:(Option.is_none config && List.is_empty enforce)
       ?below
       ~f:(fun ~context ~parent_dir ~subdirectories:_ ~files ->
         Linter.visit_directory ~dunolint_engine ~context ~parent_dir ~files))
;;
