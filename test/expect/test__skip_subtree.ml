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

let%expect_test "config" =
  let config =
    Dunolint.Config.create
      ~skip_subtree:(cond [ path (equals (Relative_path.v "foo/")), skip_subtree ])
      ~rules:
        [ cond
            [ path (equals (Relative_path.v "dune-project")), skip_subtree
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
