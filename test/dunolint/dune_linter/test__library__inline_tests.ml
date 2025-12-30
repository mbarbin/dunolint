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

(* Tests for the [inline_tests] field of [library] stanzas.

   This file focuses on testing the public API semantics of [Library.create]
   with the optional [~inline_tests:bool] parameter. These tests verify the
   three-way behavior that is specific to how the public API maps to the
   internal representation.

   Background
   ----------

   The [inline_tests] field is a simple flag field that can be present or
   absent. It may optionally have arguments (e.g., [(inline_tests (deps ./test_data))]).

   Internal representation vs Public API
   -------------------------------------

   Internally, the field uses [unit option]:
   - [Some ()] = field should be present
   - [None] = field not enforced (check [marked_for_removal] for removal)

   The public API [Library.create ~inline_tests:bool] provides three-way
   semantics:
   - [~inline_tests:true] -> [Some ()] internally (add the field if absent)
   - [~inline_tests:false] -> [None] + add to [marked_for_removal] (remove)
   - Not passing [~inline_tests] -> [None] (leave existing field unchanged)

   What this file tests
   --------------------

   - Read tests: Verify that parsing correctly detects field presence,
     including fields with arguments.
   - Create/rewrite tests: Verify the three-way public API semantics,
     including preservation of existing arguments when [~inline_tests:true].
   - Enforce tests: Verify [has_field `inline_tests] and its negation.

   General [inline_tests] coverage also exists in [Test__library], but this
   file specifically exercises the public API contract described above. *)

let parse str = Test_helpers.parse (module Dune_linter.Library) ~path:(Fpath.v "dune") str

let rewrite t str =
  let sexps_rewriter, field = Common.read str in
  Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
  print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
;;

let%expect_test "read - field absent" =
  let _, t = parse {| (library (name my_lib)) |} in
  Test_helpers.is_false (Dune_linter.Library.eval t ~predicate:(`has_field `inline_tests));
  [%expect {||}]
;;

let%expect_test "read - field present without arguments" =
  let _, t = parse {| (library (name my_lib) (inline_tests)) |} in
  Test_helpers.is_true (Dune_linter.Library.eval t ~predicate:(`has_field `inline_tests));
  [%expect {||}]
;;

let%expect_test "read - field present with arguments" =
  let _, t = parse {| (library (name my_lib) (inline_tests (deps ./test_data))) |} in
  Test_helpers.is_true (Dune_linter.Library.eval t ~predicate:(`has_field `inline_tests));
  [%expect {||}]
;;

let%expect_test "create - not passing inline_tests" =
  (* When not passing [~inline_tests], existing field is left unchanged. *)
  let t = Dune_linter.Library.create () in
  rewrite t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  rewrite t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  rewrite t {| (library (name main) (inline_tests (deps ./data))) |};
  [%expect {| (library (name main) (inline_tests (deps ./data))) |}]
;;

let%expect_test "create - inline_tests:true" =
  (* When passing [~inline_tests:true], the field is added if absent. *)
  let t = Dune_linter.Library.create ~inline_tests:true () in
  rewrite t {| (library (name main)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  (* If already present, it stays. *)
  rewrite t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  (* If present with arguments, arguments are preserved. *)
  rewrite t {| (library (name main) (inline_tests (deps ./data))) |};
  [%expect {| (library (name main) (inline_tests (deps ./data))) |}]
;;

let%expect_test "create - inline_tests:false" =
  (* When passing [~inline_tests:false], the field is removed if present. *)
  let t = Dune_linter.Library.create ~inline_tests:false () in
  rewrite t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  rewrite t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main)) |}];
  (* Also removes field with arguments. *)
  rewrite t {| (library (name main) (inline_tests (deps ./data))) |};
  [%expect {| (library (name main)) |}]
;;

open Dunolint.Config.Std

let enforce ((sexps_rewriter, field), t) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
;;

let%expect_test "enforce - has_field inline_tests (add)" =
  (* Enforcing [has_field inline_tests] adds the field if absent. *)
  let t = parse {| (library (name my_lib)) |} in
  enforce t [ has_field `inline_tests ];
  [%expect {| (library (name my_lib) (inline_tests)) |}];
  (* Does nothing if already present. *)
  let t = parse {| (library (name my_lib) (inline_tests)) |} in
  enforce t [ has_field `inline_tests ];
  [%expect {| (library (name my_lib) (inline_tests)) |}];
  (* Does nothing if present with arguments. *)
  let t = parse {| (library (name my_lib) (inline_tests (deps ./data))) |} in
  enforce t [ has_field `inline_tests ];
  [%expect {| (library (name my_lib) (inline_tests (deps ./data))) |}]
;;

let%expect_test "enforce - not (has_field inline_tests) (remove)" =
  (* Enforcing [not (has_field inline_tests)] removes the field if present. *)
  let t = parse {| (library (name my_lib)) |} in
  enforce t [ not_ (has_field `inline_tests) ];
  [%expect {| (library (name my_lib)) |}];
  let t = parse {| (library (name my_lib) (inline_tests)) |} in
  enforce t [ not_ (has_field `inline_tests) ];
  [%expect {| (library (name my_lib)) |}];
  (* Also removes field with arguments. *)
  let t = parse {| (library (name my_lib) (inline_tests (deps ./data))) |} in
  enforce t [ not_ (has_field `inline_tests) ];
  [%expect {| (library (name my_lib)) |}]
;;
