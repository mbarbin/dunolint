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

let () = add_rule (cond [ path (glob "_build/*"), skip_subtree ])
let skip_subtrees = ref []

let add_skip_subtree (condition : Dunolint.Config.Skip_subtree.Predicate.t Blang.t) =
  skip_subtrees := condition :: !skip_subtrees
;;

let () = add_skip_subtree (path (or_ (List.map ~f:glob [ ".git/*" ])))

let config () =
  let skip_subtree = cond [ or_ (List.rev !skip_subtrees), skip_subtree ] in
  let rules = List.rev !rules in
  Dunolint.Config.create ~skip_subtree ~rules ()
;;

let main =
  Command.make
    ~summary:"Generate a dunolint config for the lint-file.t test."
    (let open Command.Std in
     let+ () = Arg.return () in
     let config = config () in
     print_endline
       ";; This file is generated by [bin/lint_file_gen_config.ml]. Do not edit!";
     print_s [%sexp (config : Dunolint.Config.t)])
;;

let () =
  Cmdlang_cmdliner_runner.run main ~name:"lint-file-config-gen" ~version:"%%VERSION%%"
;;
