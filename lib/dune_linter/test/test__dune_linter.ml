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

(* A test showing how to use the [Dune_project_linter] as a library. *)

let original_contents =
  {|
(include_subdirs unqualified)

(library
  (name foo)
  (libraries a b c))

(library
  (name foofoo)
  (libraries a b c))

(executable
  (name foo)
  (libraries a b c))

(unhandled 41)

;; Atoms are ignored by dunolint (probably doesn't exists in dune).
atom
|}
;;

let print_diff t =
  let new_contents = Dune_linter.contents t in
  Expect_test_patdiff.print_patdiff original_contents new_contents ~context:3
;;

let%expect_test "lint" =
  let t =
    match
      Dune_linter.create ~path:(Relative_path.v "path/to/dune") ~original_contents
    with
    | Ok t -> t
    | Error _ -> assert false
  in
  print_diff t;
  [%expect {||}];
  print_s [%sexp (Dune_linter.path t : Relative_path.t)];
  [%expect {| path/to/dune |}];
  (* We can use the low-level sexps-rewriter API if we wish. *)
  let sexps_rewriter = Dune_linter.sexps_rewriter t in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range:_ ~file_rewriter ->
    match sexp with
    | List [ Atom "unhandled"; (Atom atom as value) ] ->
      let int = Int.of_string atom in
      File_rewriter.replace
        file_rewriter
        ~range:(Sexps_rewriter.range sexps_rewriter value)
        ~text:(Int.to_string (int + 1));
      Break
    | _ -> Continue);
  print_diff t;
  [%expect
    {|
    -13,7 +13,7
        (name foo)
        (libraries a b c))

    -|(unhandled 41)
    +|(unhandled 42)

      ;; Atoms are ignored by dunolint (probably doesn't exists in dune).
      atom
    |}];
  (* There's a typed API to access the supported stanza. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_linter.visit t ~f:(fun stanza ->
    match Dunolinter.match_stanza stanza with
    | Dune_linter.Include_subdirs s ->
      (* And use the typed getters and setters of the stanza you care about. *)
      print_s [%sexp (Dune_linter.Include_subdirs.mode s : Dune.Include_subdirs.Mode.t)];
      [%expect {| unqualified |}];
      (* If you use setters, the side effect on the memory value is done right
         away, but actual sexp rewrite is going to be registered and only
         executed during the call to [materialize] (see below). *)
      Dune_linter.Include_subdirs.set_mode s ~mode:`qualified;
      [%expect {||}];
      ()
    | _ -> ());
  print_diff t;
  [%expect
    {|
    -1,5 +1,5

    -|(include_subdirs unqualified)
    +|(include_subdirs qualified)

      (library
        (name foo)
    |}];
  (* You can also mix and match the typed API with the predicate language. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval; enforce } ->
      (match
         eval
           Dunolint.Config.Std.(
             `dune (library (name (equals (Dunolint.Dune.Library.Name.v "foofoo")))))
       with
       | False | Undefined -> ()
       | True ->
         enforce
           Dunolint.Config.Std.(
             dune (library (name (equals (Dunolint.Dune.Library.Name.v "bar")))))));
  print_diff t;
  [%expect
    {|
    -6,7 +6,7
        (libraries a b c))

      (library
    -|  (name foofoo)
    +|  (name bar)
        (libraries a b c))

      (executable
    |}];
  (* You can also use the enforcement construct from the OCaml API. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval = _; enforce = apply } ->
      let () =
        let open Dunolint.Config.Std in
        apply (dune (executable (name (equals (Dune.Executable.Name.v "my-exec")))));
        (* Enforcing unapplicable invariants has no effect. *)
        apply (dune_project (name (equals (Dune_project.Name.v "bar"))));
        apply (path (equals (Relative_path.v "path/")));
        apply (not_ (dune_project (name (equals (Dune_project.Name.v "bar")))))
      in
      ());
  print_diff t;
  [%expect
    {|
    -10,7 +10,7
        (libraries a b c))

      (executable
    -|  (name foo)
    +|  (name my-exec)
        (libraries a b c))

      (unhandled 41)
    |}];
  ()
;;
