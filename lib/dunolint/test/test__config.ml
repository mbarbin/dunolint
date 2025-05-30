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

let%expect_test "Skip_subtree.predicate" =
  let test p = Common.test_predicate (module Dunolint.Config.Skip_subtree.Predicate) p in
  test (path (equals (Relative_path.v "path/to/file")));
  [%expect {| (path (equals path/to/file)) |}];
  ()
;;

let%expect_test "skip_subtree" =
  let test p = Common.test_roundtrip (module Dunolint.Config.Skip_subtree) p in
  test return;
  [%expect {| return |}];
  test skip_subtree;
  [%expect {| skip_subtree |}];
  test (cond [ path (glob ".git/"), skip_subtree ]);
  [%expect {| (cond (((path (glob .git/)) skip_subtree))) |}];
  ()
;;

let%expect_test "empty" =
  let t = Dunolint.Config.create () in
  print_s [%sexp (Dunolint.Config.skip_subtree t : Dunolint.Config.Skip_subtree.t option)];
  [%expect {| () |}];
  print_s [%sexp (Dunolint.Config.rules t : Dunolint.Config.Rule.t list)];
  [%expect {| () |}];
  print_s [%sexp (t : Dunolint.Config.t)];
  [%expect {| ((rules ())) |}];
  require_equal [%here] (module Dunolint.Config) t t;
  [%expect {||}];
  ()
;;

let%expect_test "non-empty" =
  let t =
    Dunolint.Config.create
      ~skip_subtree:(cond [ path (glob ".git/"), skip_subtree ])
      ~rules:[ enforce (dune (has_field `instrumentation)) ]
      ()
  in
  print_s [%sexp (Dunolint.Config.skip_subtree t : Dunolint.Config.Skip_subtree.t option)];
  [%expect {| ((cond (((path (glob .git/)) skip_subtree)))) |}];
  print_s [%sexp (Dunolint.Config.rules t : Dunolint.Config.Rule.t list)];
  [%expect {| ((enforce (dune (has_field instrumentation)))) |}];
  print_s [%sexp (t : Dunolint.Config.t)];
  [%expect
    {|
    ((skip_subtree (cond (((path (glob .git/)) skip_subtree))))
     (rules ((enforce (dune (has_field instrumentation))))))
    |}];
  require_equal [%here] (module Dunolint.Config) t t;
  [%expect {||}];
  ()
;;
