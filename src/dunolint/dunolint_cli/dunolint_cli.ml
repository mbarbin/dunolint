(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let main =
  Command.group
    ~summary:"A linter for build files in OCaml dune projects."
    ~readme:(fun () ->
      "The goal of $(b,dunolint) is to check customizable invariants in your repo and \
       help with ergonomic issues, such as applying systematic changes across many \
       files. It supports things like enabling instrumentation, configuring recurring \
       lint or preprocess flags, sorting libraries alphabetically, and more. You can use \
       it at your convenience during development, and enforce consistency by integrating \
       it into your CI pipeline.\n\n\
       Main commands include:\n\n\
       - $(b,lint): apply linting configuration to an entire project at once, perhaps \
       interactively.\n\n\
       - $(b,tools): a collection of more specific commands, for example to facilitate \
       the integration with other tools.\n\n\
       For more information, use the $(b,--help) flag on a subcommand.")
    [ "lint", Cmd__lint.main
    ; ( "tools"
      , Command.group
          ~summary:"Tools commands (miscellaneous)."
          [ ( "config"
            , Command.group
                ~summary:"Utils related to config files."
                [ "validate", Cmd__tools__config__validate.main ] )
          ; "find-workspace-root", Cmd__tools__find_workspace_root.main
          ; "lint-file", Cmd__tools__lint_file.main
          ] )
    ]
;;

module Private = struct
  module Common_helpers = Common_helpers
  module Linter = Linter
end
