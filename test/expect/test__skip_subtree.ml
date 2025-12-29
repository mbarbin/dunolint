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

let%expect_test "relative_path.extend" =
  let file = Relative_path.v "dune-project" in
  let p = Relative_path.extend Relative_path.empty (Fsegment.v "dune-project") in
  print_endline (Relative_path.to_string p);
  [%expect {| dune-project |}];
  require_equal (module Relative_path) p file;
  [%expect {||}];
  ()
;;

let%expect_test "path.equals" =
  let config =
    Dunolint.Config.V1.create
      [ `skip_paths [ Dunolint.Glob.v "foo/" ]
      ; `rule
          (cond
             [ path (glob "dune-project"), return
             ; true_, enforce (dune_project (name (equals (Dune_project.Name.v "bar"))))
             ])
      ]
    |> Dunolint.Config.v1
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

let%expect_test "config skip_subtree in nested directory" =
  (* This test used to show a difference between v0's skip_subtree and v1's
     skip_paths semantic, and in particular in a cases that involves the glob
     "**".

     As seen in the glob test below "**" does not match the empty relative path.
     With [v0] we are only testing the glob conditions of [skip_subtree] on
     directories, thus with v0 we see that the directory in question is not
     skipped and thus we see a diff. Whereas with [v1] the [skip_paths]
     constructs are also applied on individual files, and thus the file
     [dune-project] in this directory gets skipped.

     mbarbin: This case confused me during a debug session, thus I was inclined
     to keep it as documentation and regression test.

     mbarbin: Edit: as we removed support for v0 entirely, we kept the part of
     the test that relates to the execution with v1 as reference. *)
  let () =
    let glob = Dunolint.Glob.v "**" in
    let test str = print_s [%sexp { is_match = (Dunolint.Glob.test glob str : bool) }] in
    test "./";
    [%expect {| ((is_match false)) |}];
    test "dune-project";
    [%expect {| ((is_match true)) |}]
  in
  Out_channel.write_all
    "dune-project"
    ~data:
      {|
(lang dune 3.18)

(name root)
|};
  Unix.mkdir "lib" 0o755;
  Out_channel.write_all
    "lib/dune-project"
    ~data:
      {|
(lang dune 3.18)

(name lib)
|};
  Unix.mkdir "lib/core" 0o755;
  let run_test () =
    Err.For_test.protect (fun () ->
      let dunolint_engine = Dunolint_engine.create ~running_mode:Dry_run () in
      let () =
        Dunolint_engine.visit
          dunolint_engine
          ~f:(fun ~context ~parent_dir ~subdirectories:_ ~files ->
            Dunolint_cli.Private.Linter.visit_directory
              ~dunolint_engine
              ~context
              ~parent_dir
              ~files)
      in
      Dunolint_engine.materialize dunolint_engine)
  in
  (* With v1. *)
  Out_channel.write_all
    "lib/core/dunolint"
    ~data:
      {|(lang dunolint 1.0)

(skip_paths "**")

(rule (enforce (dune_project (name (equals test)))))
|};
  Out_channel.write_all
    "lib/core/dune-project"
    ~data:
      {|
(lang dune 3.18)

(name core)
|};
  run_test ();
  [%expect {||}];
  ()
;;
