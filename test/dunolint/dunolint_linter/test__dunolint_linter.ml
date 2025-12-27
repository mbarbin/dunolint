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

(* A test showing how to use the [Dunolint_linter] as a library. *)

let original_contents =
  {|
(lang dunolint 1.0)

(skip_paths .git/)

(rule (enforce (dune (has_field instrumentation))))

;; Atoms are ignored by dunolint (probably doesn't exists in dunolint).
atom
|}
  |> String.lstrip
;;

let print_diff t =
  let new_contents = Dunolint_linter.contents t in
  Expect_test_patdiff.print_patdiff original_contents new_contents ~context:3
;;

let%expect_test "lint" =
  let path = Relative_path.v "path/to/dunolint" in
  let t =
    match Dunolint_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  print_diff t;
  [%expect {||}];
  print_s [%sexp (Dunolint_linter.path t : Relative_path.t)];
  [%expect {| path/to/dunolint |}];
  (* There's a typed API to access the supported stanza. *)
  Dunolint_linter.visit t ~f:(fun stanza ->
    match Dunolinter.match_stanza stanza with
    | Dunolint_linter.Dunolint_lang_version s ->
      (* And use the typed getters and setters of the stanza you care about. *)
      print_s
        [%sexp
          (Dunolint_linter.Dunolint_lang_version.dunolint_lang_version s
           : Dunolint0.Dunolint_lang_version.t)];
      [%expect {| 1.0 |}];
      (* If you use setters, the side effect on the memory value is done right
         away, but actual sexp rewrite is going to be registered and only
         executed during the call to [materialize] (see below). *)
      Dunolint_linter.Dunolint_lang_version.set_dunolint_lang_version
        s
        ~dunolint_lang_version:(Dunolint0.Dunolint_lang_version.create (1, 1));
      [%expect {||}];
      ()
    | _ -> ());
  print_diff t;
  [%expect
    {|
    -1,4 +1,4
    -|(lang dunolint 1.0)
    +|(lang dunolint 1.1)

      (skip_paths .git/)
    |}];
  (* You can also mix and match the typed API with the predicate language. *)
  let sexps_rewriter = Dunolint_linter.sexps_rewriter t in
  Sexps_rewriter.reset sexps_rewriter;
  Dunolint_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval; enforce = _ } ->
      (match
         eval
           ~path
           ~predicate:
             Dunolint.Config.Std.(
               `dunolint
                 (dunolint_lang_version
                    (eq (Dunolint0.Dunolint_lang_version.create (1, 0)))))
       with
       | False | Undefined -> assert false
       | True ->
         let original_sexp = Dunolinter.original_sexp stanza in
         print_s original_sexp;
         [%expect {| (lang dunolint 1.0) |}];
         ()));
  (* You can also use the enforcement construct from the OCaml API. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dunolint_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval = _; enforce } ->
      let apply condition = enforce ~path ~condition in
      let () =
        let open Dunolint.Config.Std in
        apply
          (dunolint
             (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))));
        (* Enforcing unapplicable invariants has no effect. *)
        apply (dune (library (name (equals (Dune.Library.Name.v "bar")))));
        apply (dune_project (name (equals (Dune_project.Name.v "foo"))));
        apply (path (glob "path/"));
        apply (not_ (dune (library (name (equals (Dune.Library.Name.v "bar"))))));
        apply (not_ (dune_project (name (equals (Dune_project.Name.v "foo")))))
      in
      ());
  print_diff t;
  [%expect
    {|
    -1,4 +1,4
    -|(lang dunolint 1.0)
    +|(lang dunolint 1.5)

      (skip_paths .git/)
    |}];
  ()
;;

let%expect_test "visit with invalid stanza" =
  let path = Relative_path.v "path/to/dunolint" in
  let original_contents =
    {|
(lang dunolint INVALID)

(skip_paths .git/)
|}
    |> String.lstrip
  in
  let t =
    match Dunolint_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  (* Visiting a dunolint file with an invalid lang stanza emits an error. *)
  Err.For_test.protect (fun () -> Dunolint_linter.visit t ~f:(fun _ -> ()));
  [%expect
    {|
    File "path/to/dunolint", line 1, characters 15-22:
    Error: Expected VERSION.MINOR format, got: "INVALID".
    [123]
    |}];
  ()
;;
