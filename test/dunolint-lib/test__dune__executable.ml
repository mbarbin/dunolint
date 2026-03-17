(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Executable.Predicate.equal in
  let has_field_a = `has_field `instrumentation in
  let has_field_b = `has_field `lint in
  let instrumentation_a =
    `instrumentation (Blang.base (`backend (Dune.Instrumentation.Backend.v "bisect_ppx")))
  in
  let libraries_a =
    `libraries
      (Blang.base (`mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]))
  in
  let libraries_b = `libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ])) in
  let lint_a = `lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))) in
  let name_a = `name (Blang.base (`equals (Dune.Executable.Name.v "main"))) in
  let preprocess_a =
    `preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))
  in
  let public_name_a =
    `public_name (Blang.base (`equals (Dune.Executable.Public_name.v "main")))
  in
  (* Physical equality. *)
  require (equal instrumentation_a instrumentation_a);
  [%expect {||}];
  require (equal libraries_a libraries_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`has_field `instrumentation) (`has_field `instrumentation));
  [%expect {||}];
  require
    (equal
       (`instrumentation
           (Blang.base (`backend (Dune.Instrumentation.Backend.v "bisect_ppx"))))
       (`instrumentation
           (Blang.base (`backend (Dune.Instrumentation.Backend.v "bisect_ppx")))));
  [%expect {||}];
  require
    (equal
       (`libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ])))
       (`libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ]))));
  [%expect {||}];
  require
    (equal
       (`lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))))
       (`lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))));
  [%expect {||}];
  require
    (equal
       (`name (Blang.base (`equals (Dune.Executable.Name.v "main"))))
       (`name (Blang.base (`equals (Dune.Executable.Name.v "main")))));
  [%expect {||}];
  require
    (equal
       (`preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))))
       (`preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))));
  [%expect {||}];
  require
    (equal
       (`public_name (Blang.base (`equals (Dune.Executable.Public_name.v "main"))))
       (`public_name (Blang.base (`equals (Dune.Executable.Public_name.v "main")))));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal has_field_a has_field_b));
  [%expect {||}];
  require (not (equal libraries_a libraries_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal has_field_a instrumentation_a));
  [%expect {||}];
  require (not (equal instrumentation_a libraries_a));
  [%expect {||}];
  require (not (equal libraries_a lint_a));
  [%expect {||}];
  require (not (equal lint_a name_a));
  [%expect {||}];
  require (not (equal name_a preprocess_a));
  [%expect {||}];
  require (not (equal preprocess_a public_name_a));
  [%expect {||}];
  require (not (equal public_name_a has_field_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Executable.Predicate) p in
  test (has_field `instrumentation);
  [%expect {| (has_field instrumentation) |}];
  test (has_field `lint);
  [%expect {| (has_field lint) |}];
  test (has_field `name);
  [%expect {| (has_field name) |}];
  test (has_field `preprocess);
  [%expect {| (has_field preprocess) |}];
  test (has_field `public_name);
  [%expect {| (has_field public_name) |}];
  test (instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx")));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* libraries predicate. *)
  test (libraries (mem [ Dune.Library.Name.v "base" ]));
  [%expect {| (libraries (mem base)) |}];
  test (libraries (mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]));
  [%expect {| (libraries (mem base core)) |}];
  test (lint (pps (pp (Dune.Pp.Name.v "ppx_compare"))));
  [%expect {| (lint (pps (pp ppx_compare))) |}];
  test (name (equals (Dune.Executable.Name.v "main")));
  [%expect {| (name (equals main)) |}];
  test (preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare"))));
  [%expect {| (preprocess (pps (pp ppx_compare))) |}];
  test (public_name (equals (Dune.Executable.Public_name.v "dunolint")));
  [%expect {| (public_name (equals dunolint)) |}];
  ()
;;
