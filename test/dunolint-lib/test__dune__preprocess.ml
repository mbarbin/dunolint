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
  let equal = Dune.Preprocess.Predicate.equal in
  let no_preprocessing = `no_preprocessing in
  let pps_a = `pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_compare"))) in
  let pps_b = `pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_sexp_conv"))) in
  (* Physical equality. *)
  require (equal pps_a pps_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal `no_preprocessing `no_preprocessing);
  [%expect {||}];
  require
    (equal
       (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_compare"))))
       (`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_compare")))));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal pps_a pps_b));
  [%expect {||}];
  (* Different variants. *)
  require (not (equal no_preprocessing pps_a));
  [%expect {||}];
  require (not (equal pps_a no_preprocessing));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Preprocess.Predicate) p in
  test (pps (pp (Dune.Pp.Name.v "ppx_compare")));
  [%expect {| (pps (pp ppx_compare)) |}];
  test no_preprocessing;
  [%expect {| no_preprocessing |}];
  ()
;;
