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

module Unix = UnixLabels

let%expect_test "config cache" =
  (* This test verifies that the config cache works correctly by building
     contexts multiple times for files in similar directories. We check that:
     1. Multiple configs at different levels are loaded correctly
     2. The context contains the expected number of configs
     3. Building contexts for multiple files in the same directory gives
     consistent results (demonstrating caching behavior). *)
  Err.For_test.protect (fun () ->
    (* Create a test directory structure with config files at different levels. *)
    Unix.mkdir "test-config-cache" ~perm:0o755;
    Unix.mkdir "test-config-cache/lib" ~perm:0o755;
    Unix.mkdir "test-config-cache/lib/subdir" ~perm:0o755;
    (* Write dunolint config files at root and lib levels. *)
    Out_channel.write_all
      "test-config-cache/dunolint"
      ~data:
        {|(lang dunolint 1.0)

(skip_paths **/excluded/)
|};
    Out_channel.write_all
      "test-config-cache/lib/dunolint"
      ~data:
        {|(lang dunolint 1.0)

(skip_paths **/temp/)
|};
    (* Create the engine which will use the config cache internally. *)
    let t = Dunolint_engine.create ~running_mode:Dry_run () in
    (* Build context for the first file in lib/subdir.
       This should load configs from both test-config-cache/ and test-config-cache/lib/. *)
    let ctx1 =
      Dunolint_engine.build_context
        t
        ~path:(Relative_path.v "test-config-cache/lib/subdir/file1.ml")
    in
    let configs1 = Dunolint_engine.Context.configs ctx1 in
    print_s [%sexp (List.length configs1 : int)];
    [%expect {| 2 |}];
    (* Verify the configs are at the expected locations. *)
    List.iter configs1 ~f:(fun { config = _; location } ->
      print_s [%sexp (location : Relative_path.t)]);
    [%expect
      {|
      test-config-cache/
      test-config-cache/lib/
      |}];
    (* Build context for a second file in the same directory.
       The cached configs should be used, giving the same result. *)
    let ctx2 =
      Dunolint_engine.build_context
        t
        ~path:(Relative_path.v "test-config-cache/lib/subdir/file2.ml")
    in
    let configs2 = Dunolint_engine.Context.configs ctx2 in
    print_s [%sexp (List.length configs2 : int)];
    [%expect {| 2 |}];
    (* Verify that the configs are physically the same (from cache). *)
    List.iter2_exn configs1 configs2 ~f:(fun c1 c2 ->
      require [%here] (phys_equal c1.config c2.config));
    (* Build context for a third file in the same directory. *)
    let ctx3 =
      Dunolint_engine.build_context
        t
        ~path:(Relative_path.v "test-config-cache/lib/subdir/file3.ml")
    in
    let configs3 = Dunolint_engine.Context.configs ctx3 in
    print_s [%sexp (List.length configs3 : int)];
    [%expect {| 2 |}];
    (* Verify that configs from ctx3 are also physically the same as ctx1. *)
    List.iter2_exn configs1 configs3 ~f:(fun c1 c3 ->
      require [%here] (phys_equal c1.config c3.config));
    (* Build context for a file directly in lib (not in subdir).
       This should also have 2 configs (root and lib). *)
    let ctx4 =
      Dunolint_engine.build_context t ~path:(Relative_path.v "test-config-cache/lib/dune")
    in
    let configs4 = Dunolint_engine.Context.configs ctx4 in
    print_s [%sexp (List.length configs4 : int)];
    [%expect {| 2 |}];
    (* Verify that configs from ctx4 are also physically the same as ctx1. *)
    List.iter2_exn configs1 configs4 ~f:(fun c1 c4 ->
      require [%here] (phys_equal c1.config c4.config));
    (* Build context for a file at the root level.
       This should only have 1 config (from root). *)
    let ctx5 =
      Dunolint_engine.build_context
        t
        ~path:(Relative_path.v "test-config-cache/dune-project")
    in
    let configs5 = Dunolint_engine.Context.configs ctx5 in
    print_s [%sexp (List.length configs5 : int)];
    [%expect {| 1 |}];
    List.iter configs5 ~f:(fun { config = _; location } ->
      print_s [%sexp (location : Relative_path.t)]);
    [%expect {| test-config-cache/ |}];
    (* Verify that the single config from ctx5 is physically the same as the
       first config from ctx1 (both are the root config from the cache). *)
    let ctx1_prefix = List.take configs1 (List.length configs5) in
    List.iter2_exn configs5 ctx1_prefix ~f:(fun c5 c1 ->
      require [%here] (phys_equal c5.config c1.config));
    ());
  [%expect {||}];
  ()
;;
