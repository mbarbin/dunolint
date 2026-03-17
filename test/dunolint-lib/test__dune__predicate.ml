(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "equal" =
  let equal = Dune.Predicate.equal in
  let executable_a =
    `executable
      (Blang.base (`name (Blang.base (`equals (Dune.Executable.Name.v "main")))))
  in
  let executable_b =
    `executable
      (Blang.base (`name (Blang.base (`equals (Dune.Executable.Name.v "test")))))
  in
  let has_field_a = `has_field `instrumentation in
  let include_subdirs_a = `include_subdirs Blang.true_ in
  let instrumentation_a = `instrumentation Blang.true_ in
  let libraries_a =
    `libraries
      (Blang.base (`mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]))
  in
  let libraries_b = `libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ])) in
  let library_a = `library Blang.true_ in
  let lint_a = `lint Blang.true_ in
  let preprocess_a = `preprocess Blang.true_ in
  let stanza_a = `stanza (Blang.base `library) in
  (* Physical equality. *)
  require (equal executable_a executable_a);
  [%expect {||}];
  require (equal libraries_a libraries_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (`executable
           (Blang.base (`name (Blang.base (`equals (Dune.Executable.Name.v "main"))))))
       (`executable
           (Blang.base (`name (Blang.base (`equals (Dune.Executable.Name.v "main")))))));
  [%expect {||}];
  require (equal (`has_field `instrumentation) (`has_field `instrumentation));
  [%expect {||}];
  require (equal (`has_field `lint) (`has_field `lint));
  [%expect {||}];
  require (equal (`include_subdirs Blang.true_) (`include_subdirs Blang.true_));
  [%expect {||}];
  require (equal (`instrumentation Blang.true_) (`instrumentation Blang.true_));
  [%expect {||}];
  require
    (equal
       (`libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ])))
       (`libraries (Blang.base (`mem [ Dune.Library.Name.v "base" ]))));
  [%expect {||}];
  require (equal (`library Blang.true_) (`library Blang.true_));
  [%expect {||}];
  require (equal (`lint Blang.true_) (`lint Blang.true_));
  [%expect {||}];
  require (equal (`preprocess Blang.true_) (`preprocess Blang.true_));
  [%expect {||}];
  require (equal (`stanza (Blang.base `library)) (`stanza (Blang.base `library)));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal executable_a executable_b));
  [%expect {||}];
  require (not (equal libraries_a libraries_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal executable_a has_field_a));
  [%expect {||}];
  require (not (equal has_field_a include_subdirs_a));
  [%expect {||}];
  require (not (equal include_subdirs_a instrumentation_a));
  [%expect {||}];
  require (not (equal instrumentation_a libraries_a));
  [%expect {||}];
  require (not (equal libraries_a library_a));
  [%expect {||}];
  require (not (equal library_a lint_a));
  [%expect {||}];
  require (not (equal lint_a preprocess_a));
  [%expect {||}];
  require (not (equal preprocess_a stanza_a));
  [%expect {||}];
  require (not (equal stanza_a executable_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Predicate) p in
  test (executable (name (equals (Dune.Executable.Name.v "main"))));
  [%expect {| (executable (name (equals main))) |}];
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
  test (include_subdirs (equals `unqualified));
  [%expect {| (include_subdirs (equals unqualified)) |}];
  test (instrumentation (backend (Dune.Instrumentation.Backend.v "bisect_ppx")));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* libraries predicate. *)
  test (libraries (mem [ Dune.Library.Name.v "base" ]));
  [%expect {| (libraries (mem base)) |}];
  test (libraries (mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]));
  [%expect {| (libraries (mem base core)) |}];
  test (library (name (equals (Dune.Library.Name.v "main"))));
  [%expect {| (library (name (equals main))) |}];
  test (lint (pps (pp (Dune.Pp.Name.v "ppx_equal"))));
  [%expect {| (lint (pps (pp ppx_equal))) |}];
  test (preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare"))));
  [%expect {| (preprocess (pps (pp ppx_compare))) |}];
  test (stanza (Blang.base `library));
  [%expect {| (stanza library) |}];
  ()
;;
