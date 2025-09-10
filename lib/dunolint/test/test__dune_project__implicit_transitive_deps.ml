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

let%expect_test "all" =
  List.iter Dune_project.Implicit_transitive_deps.Value.all ~f:(fun value ->
    print_s [%sexp (value : Dune_project.Implicit_transitive_deps.Value.t)]);
  [%expect
    {|
    true
    false
    false-if-hidden-includes-supported
    |}];
  ()
;;

let%expect_test "of_sexp" =
  let test sexp =
    let t = [%of_sexp: Dune_project.Implicit_transitive_deps.Value.t] sexp in
    let s = [%sexp (t : Dune_project.Implicit_transitive_deps.Value.t)] in
    require_equal [%here] (module Sexp) sexp s
  in
  test (Atom "true");
  test (Atom "false");
  test (Atom "false-if-hidden-includes-supported");
  [%expect {||}];
  require_does_raise [%here] (fun () -> test (Atom "something else"));
  [%expect {| (Failure "Invalid implicit_transitive_deps value: something else") |}];
  require_does_raise [%here] (fun () ->
    test (List [ Atom "not"; Atom "an"; Atom "atom" ]));
  [%expect {| (Failure "Expected atom for implicit_transitive_deps value") |}];
  ()
;;

let%expect_test "predicate" =
  let test p =
    Common.test_predicate (module Dune_project.Implicit_transitive_deps.Predicate) p
  in
  test (equals `True);
  [%expect {| (equals true) |}];
  test (equals `False);
  [%expect {| (equals false) |}];
  test (equals `False_if_hidden_includes_supported);
  [%expect {| (equals false-if-hidden-includes-supported) |}];
  ()
;;

let%expect_test "sort" =
  let sort ts =
    List.sort ts ~compare:Dune_project.Implicit_transitive_deps.Value.compare
  in
  let test ts =
    print_s [%sexp (sort ts : Dune_project.Implicit_transitive_deps.Value.t list)]
  in
  test [ `False; `True; `False_if_hidden_includes_supported ];
  [%expect {| (true false false-if-hidden-includes-supported) |}];
  require
    [%here]
    (List.equal
       Dune_project.Implicit_transitive_deps.Value.equal
       Dune_project.Implicit_transitive_deps.Value.all
       (sort Dune_project.Implicit_transitive_deps.Value.all));
  [%expect {||}];
  ()
;;
