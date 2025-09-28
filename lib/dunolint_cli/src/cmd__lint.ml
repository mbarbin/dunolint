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
     let config =
       Common_helpers.load_config_opt_exn ~config ~append_extra_rules:enforce
     in
     Dunolint_engine.run ~running_mode
     @@ fun dunolint_engine ->
     Dunolint_engine.visit
       dunolint_engine
       ?below
       ~f:(fun ~parent_dir ~subdirectories:_ ~files ->
         Linter.visit_directory ~dunolint_engine ~config ~parent_dir ~files))
;;
