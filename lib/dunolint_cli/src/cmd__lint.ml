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
    ~summary:"lint project"
    (let%map_open.Command dunolint_engine_config = Dunolint_engine.Config.arg
     and () = Log_cli.set_config ()
     and config =
       Arg.named_opt [ "config" ] Param.file ~doc:"Path to dunolint config file"
     and below = Common_helpers.below ~doc:"Lint only below this path"
     and enforce =
       Arg.named_multi
         [ "enforce" ]
         (Common_helpers.sexpable_param (module Dunolint.Condition))
         ~docv:"COND"
         ~doc:"Add condition to enforce"
       >>| List.map ~f:(fun condition -> `enforce condition)
     in
     let config =
       match config with
       | Some config ->
         let contents = In_channel.read_all config in
         Parsexp.Conv_single.parse_string_exn contents Dunolint.Config.t_of_sexp
       | None ->
         Dunolint.Config.create
           ~skip_subtree:(Common_helpers.skip_subtree ~globs:[])
           ~rules:[]
           ()
     in
     let config =
       Dunolint.Config.create
         ?skip_subtree:(Dunolint.Config.skip_subtree config)
         ~rules:(Dunolint.Config.rules config @ enforce)
         ()
     in
     Dunolint_engine.run ~config:dunolint_engine_config
     @@ fun dunolint_engine ->
     Dunolint_engine.visit
       dunolint_engine
       ?below
       ~f:(fun ~parent_dir ~subdirectories:_ ~files ->
         Linter.visit_directory ~dunolint_engine ~config ~parent_dir ~files))
;;
