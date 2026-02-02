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

let%expect_test "Predicate.equal" =
  let equal = Dune.Library.Predicate.equal in
  let has_field_a = `has_field `instrumentation in
  let has_field_b = `has_field `lint in
  let instrumentation_a =
    `instrumentation
      (Blang.base (`backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")))
  in
  let lint_a = `lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))) in
  let modes_a = `modes (Blang.base (`has_mode `melange)) in
  let name_a = `name (Blang.base (`equals (Dune.Library.Name.v "main"))) in
  let package_a = `package (Blang.base (`equals (Dune.Package.Name.v "pkg"))) in
  let preprocess_a =
    `preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))
  in
  let public_name_a =
    `public_name (Blang.base (`equals (Dune.Library.Public_name.v "main")))
  in
  let if_present_a =
    `if_present (`package (Blang.base (`equals (Dune.Package.Name.v "pkg_a"))))
  in
  let if_present_b =
    `if_present (`package (Blang.base (`equals (Dune.Package.Name.v "pkg_b"))))
  in
  let if_present_pn =
    `if_present (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "pn"))))
  in
  (* Physical equality. *)
  require (equal name_a name_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`has_field `instrumentation) (`has_field `instrumentation));
  [%expect {||}];
  require
    (equal
       (`instrumentation
           (Blang.base (`backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))))
       (`instrumentation
           (Blang.base (`backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")))));
  [%expect {||}];
  require
    (equal
       (`lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))))
       (`lint (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))));
  [%expect {||}];
  require
    (equal
       (`modes (Blang.base (`has_mode `melange)))
       (`modes (Blang.base (`has_mode `melange))));
  [%expect {||}];
  require
    (equal
       (`name (Blang.base (`equals (Dune.Library.Name.v "main"))))
       (`name (Blang.base (`equals (Dune.Library.Name.v "main")))));
  [%expect {||}];
  require
    (equal
       (`package (Blang.base (`equals (Dune.Package.Name.v "pkg"))))
       (`package (Blang.base (`equals (Dune.Package.Name.v "pkg")))));
  [%expect {||}];
  require
    (equal
       (`preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a"))))))
       (`preprocess (Blang.base (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_a")))))));
  [%expect {||}];
  require
    (equal
       (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "main"))))
       (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "main")))));
  [%expect {||}];
  require
    (equal
       (`if_present (`package (Blang.base (`equals (Dune.Package.Name.v "pkg")))))
       (`if_present (`package (Blang.base (`equals (Dune.Package.Name.v "pkg"))))));
  [%expect {||}];
  require
    (equal
       (`if_present
           (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "pn")))))
       (`if_present
           (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "pn"))))));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal has_field_a has_field_b));
  [%expect {||}];
  require (not (equal if_present_a if_present_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal has_field_a instrumentation_a));
  [%expect {||}];
  require (not (equal instrumentation_a lint_a));
  [%expect {||}];
  require (not (equal lint_a modes_a));
  [%expect {||}];
  require (not (equal modes_a name_a));
  [%expect {||}];
  require (not (equal name_a package_a));
  [%expect {||}];
  require (not (equal package_a preprocess_a));
  [%expect {||}];
  require (not (equal preprocess_a public_name_a));
  [%expect {||}];
  require (not (equal public_name_a if_present_a));
  [%expect {||}];
  require (not (equal if_present_a has_field_a));
  [%expect {||}];
  (* Test each [if_present] variant as first argument to cover the nested catch-all. *)
  require (not (equal if_present_a if_present_pn));
  [%expect {||}];
  require (not (equal if_present_pn if_present_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Library.Predicate) p in
  test (has_field `inline_tests);
  [%expect {| (has_field inline_tests) |}];
  test (has_field `instrumentation);
  [%expect {| (has_field instrumentation) |}];
  test (has_field `lint);
  [%expect {| (has_field lint) |}];
  test (has_field `modes);
  [%expect {| (has_field modes) |}];
  test (has_field `name);
  [%expect {| (has_field name) |}];
  test (has_field `package);
  [%expect {| (has_field package) |}];
  test (has_field `preprocess);
  [%expect {| (has_field preprocess) |}];
  test (has_field `public_name);
  [%expect {| (has_field public_name) |}];
  test (instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  test (lint (pps (pp (Dune.Pp.Name.v "ppx_compare"))));
  [%expect {| (lint (pps (pp ppx_compare))) |}];
  test (modes (mem [ `melange ]));
  [%expect {| (modes (mem melange)) |}];
  test (name (equals (Dune.Library.Name.v "main")));
  [%expect {| (name (equals main)) |}];
  test (package (equals (Dune.Package.Name.v "my_package")));
  [%expect {| (package (equals my_package)) |}];
  test (preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare"))));
  [%expect {| (preprocess (pps (pp ppx_compare))) |}];
  test (public_name (equals (Dune.Library.Public_name.v "dunolint")));
  [%expect {| (public_name (equals dunolint)) |}];
  test (if_present (`package (Blang.base (`equals (Dune.Package.Name.v "my_package")))));
  [%expect {| (if_present (package (equals my_package))) |}];
  test
    (if_present
       (`public_name (Blang.base (`equals (Dune.Library.Public_name.v "dunolint")))));
  [%expect {| (if_present (public_name (equals dunolint))) |}];
  ()
;;
