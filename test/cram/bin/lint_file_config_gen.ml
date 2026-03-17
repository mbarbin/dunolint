(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
