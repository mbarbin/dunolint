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

open! Dunolint.Config.Std

(* This test covers the evaluation of the [path.equals] predicate. We cover two
   different execution context for it:

   1. From within the [skip_subtree] config section. In this case, the paths
   that are applied to the [path] selector are the directory paths of the
   directory that are being visited. Thus, the path ends with a '/' character.

   2. From within the rules. In this case, the path evaluated is the path of the
   file that is linted, thus would have a basename of [dune-project] or [dune].
   It is the complete relative path from the root of the repository. *)

let%expect_test "path.equals" =
  let config =
    Dunolint.Config.create
      ~skip_subtree:(cond [ path (equals (Relative_path.v "foo/")), skip_subtree ])
      ~rules:
        [ cond
            [ path (equals (Relative_path.v "dune-project")), return
            ; true_, enforce (dune_project (name (equals (Dune_project.Name.v "bar"))))
            ]
        ]
      ()
  in
  Unix.mkdir "foo" 0o755;
  Out_channel.write_all
    "foo/dune-project"
    ~data:
      {|
(lang dune 3.18)

(name foo)
|};
  Unix.mkdir "bar" 0o755;
  Out_channel.write_all
    "bar/dune-project"
    ~data:
      {|
(lang dune 3.18)

(name foo_bar)
|};
  Out_channel.write_all
    "dune-project"
    ~data:
      {|
(lang dune 3.18)

(name root)
|};
  Err.For_test.protect (fun () -> Test_helpers.run_linter ~config);
  [%expect
    {|
    dry-run: Would edit file "bar/dune-project":
    -1,4 +1,4

      (lang dune 3.18)

    -|(name foo_bar)
    +|(name bar)
    |}];
  ()
;;
