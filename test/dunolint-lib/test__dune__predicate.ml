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
  let library_a = `library Blang.true_ in
  let lint_a = `lint Blang.true_ in
  let preprocess_a = `preprocess Blang.true_ in
  let stanza_a = `stanza (Blang.base `library) in
  (* Physical equality. *)
  require (equal executable_a executable_a);
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
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal executable_a has_field_a));
  [%expect {||}];
  require (not (equal has_field_a include_subdirs_a));
  [%expect {||}];
  require (not (equal include_subdirs_a instrumentation_a));
  [%expect {||}];
  require (not (equal instrumentation_a library_a));
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
  test (instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
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
