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

open Dunolint.Config.Std

let rules = ref []

type dune_rule = (Dunolint.Predicate.t, Dunolint.Condition.t) Dunolint.Rule.t

let add_rule cs = rules := (cs : dune_rule) :: !rules

let () =
  add_rule
    (cond
       [ path (glob "vendor/*"), return
       ; true_, enforce (dune_project (name (equals (Dune_project.Name.v "foo"))))
       ])
;;

let skip_paths_ref = ref []
let skip_paths (globs : string list) = skip_paths_ref := globs :: !skip_paths_ref
let () = skip_paths [ ".git/*" ]

module Format = struct
  type t = [ `v1 ]

  let all = [ `v1 ]

  let to_string = function
    | `v1 -> "v1"
  ;;
end

let config_v1 () =
  skip_paths [ "_build/*" ];
  let skip_paths =
    List.rev_map !skip_paths_ref ~f:(fun globs ->
      `skip_paths (List.map globs ~f:Dunolint.Glob.v))
  in
  let rules = List.rev_map !rules ~f:(fun rule -> `rule rule) in
  Dunolint.Config.v1 (Dunolint.Config.V1.create (skip_paths @ rules))
;;

let main =
  Command.make
    ~summary:"Generate a dunolint config for the lint-file.t test."
    (let open Command.Std in
     let+ format =
       Arg.named
         [ "format" ]
         (Param.enumerated (module Format))
         ~docv:"Format"
         ~doc:"Which format of config to use."
     in
     let config =
       match format with
       | `v1 -> config_v1 ()
     in
     print_endline
       (Dunolint.Config.to_file_contents
          config
          ~generated_by:"bin/lint_file_gen_config.ml"))
;;

let () =
  Cmdlang_cmdliner_err_runner.run main ~name:"lint-file-config-gen" ~version:"%%VERSION%%"
;;
