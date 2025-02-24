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

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Pps.Predicate) p in
  test (pp (Dune.Pp.Name.v "ppx_compare"));
  [%expect {| (pp ppx_compare) |}];
  test (flag { name = "-a"; param = `any; applies_to = `any });
  [%expect
    {|
    (flag (
      (name       -a)
      (param      any)
      (applies_to any)))
    |}];
  test (flag { name = "-a"; param = `none; applies_to = `driver });
  [%expect
    {|
    (flag (
      (name       -a)
      (param      none)
      (applies_to driver)))
    |}];
  test
    (flag
       { name = "-a"; param = `some; applies_to = `pp (Dune.Pp.Name.v "ppx_js_style") });
  [%expect
    {|
    (flag (
      (name  -a)
      (param some)
      (applies_to (pp ppx_js_style))))
    |}];
  test
    (flag
       { name = "-unused-code-warnings"; param = `equals "force"; applies_to = `driver });
  [%expect
    {|
    (flag (
      (name -unused-code-warnings) (param (equals force)) (applies_to driver)))
    |}];
  test
    (pp_with_flag
       { pp = Dune.Pp.Name.v "ppx_js_style"
       ; flag = "-allow-let-operators"
       ; param = `none
       });
  [%expect
    {|
    (pp_with_flag (
      (pp    ppx_js_style)
      (flag  -allow-let-operators)
      (param none)))
    |}];
  ()
;;
