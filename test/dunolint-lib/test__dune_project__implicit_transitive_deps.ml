(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune_project.Implicit_transitive_deps.Predicate.equal in
  let equals_true = `equals `True in
  let equals_false = `equals `False in
  (* Physical equality. *)
  require (equal equals_true equals_true);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`equals `True) (`equals `True));
  [%expect {||}];
  require (equal (`equals `False) (`equals `False));
  [%expect {||}];
  require
    (equal
       (`equals `False_if_hidden_includes_supported)
       (`equals `False_if_hidden_includes_supported));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal equals_true equals_false));
  [%expect {||}];
  ()
;;

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
    require_equal (module Sexp) sexp s
  in
  test (Atom "true");
  test (Atom "false");
  test (Atom "false-if-hidden-includes-supported");
  [%expect {||}];
  require_does_raise (fun () -> test (Atom "something else"));
  [%expect
    {|
    (Of_sexp_error "Unsupported implicit_transitive_deps value [something else]."
     (invalid_sexp "something else"))
    |}];
  require_does_raise (fun () -> test (List [ Atom "not"; Atom "an"; Atom "atom" ]));
  [%expect
    {|
    (Of_sexp_error "Expected atom for implicit_transitive_deps value."
     (invalid_sexp (not an atom)))
    |}];
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
    (List.equal
       Dune_project.Implicit_transitive_deps.Value.equal
       Dune_project.Implicit_transitive_deps.Value.all
       (sort Dune_project.Implicit_transitive_deps.Value.all));
  [%expect {||}];
  ()
;;
