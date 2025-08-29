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
          [ "lint-file", Cmd__tools__lint_file.main ] )
    ]
;;

module Private = struct
  module Linter = Linter
end
