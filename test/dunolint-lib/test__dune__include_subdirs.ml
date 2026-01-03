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

let%expect_test "Mode.equal" =
  let equal = Dune.Include_subdirs.Mode.equal in
  (* Structural equality - same variant. *)
  require (equal `no `no);
  [%expect {||}];
  require (equal `unqualified `unqualified);
  [%expect {||}];
  require (equal `qualified `qualified);
  [%expect {||}];
  (* Different variants. *)
  require (not (equal `no `unqualified));
  [%expect {||}];
  ()
;;

let%expect_test "Predicate.equal" =
  let equal = Dune.Include_subdirs.Predicate.equal in
  let equals_no = `equals `no in
  let equals_unqualified = `equals `unqualified in
  (* Physical equality. *)
  require (equal equals_no equals_no);
  [%expect {||}];
  (* Structural equality - same variant, same inner value. *)
  require (equal (`equals `no) (`equals `no));
  [%expect {||}];
  require (equal (`equals `unqualified) (`equals `unqualified));
  [%expect {||}];
  require (equal (`equals `qualified) (`equals `qualified));
  [%expect {||}];
  (* Same variant, different inner value. *)
  require (not (equal equals_no equals_unqualified));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Include_subdirs.Predicate) p in
  test (equals `no);
  [%expect {| (equals no) |}];
  test (equals `unqualified);
  [%expect {| (equals unqualified) |}];
  test (equals `qualified);
  [%expect {| (equals qualified) |}];
  ()
;;
