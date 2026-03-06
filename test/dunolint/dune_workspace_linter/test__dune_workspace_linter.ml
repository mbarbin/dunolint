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

(* A test showing how to use the [Dune_workspace_linter] as a library. *)

let original_contents =
  {|
(lang dune 3.17)

;; Atoms are ignored by dunolint (probably doesn't exists in dune).
atom
|}
  |> String.lstrip
;;

let print_diff t =
  let new_contents = Dune_workspace_linter.contents t in
  Myers.print_diff original_contents new_contents ~context:3
;;

let%expect_test "lint" =
  let path = Relative_path.v "dune-workspace" in
  let t =
    match Dune_workspace_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  print_diff t;
  [%expect
    {|
    --- expected
    +++ actual
    |}];
  print_s [%sexp (Dune_workspace_linter.path t : Relative_path.t)];
  [%expect {| dune-workspace |}];
  print_s [%sexp (List.length (Dune_workspace_linter.original_sexps t) : int)];
  [%expect {| 2 |}];
  (* We can use the low-level sexps-rewriter API if we wish. *)
  let sexps_rewriter = Dune_workspace_linter.sexps_rewriter t in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "3.17" ->
      File_rewriter.replace file_rewriter ~range ~text:"3.19";
      Break
    | _ -> Continue);
  print_diff t;
  [%expect
    {|
    --- expected
    +++ actual
    @@ -1,4 +1,4 @@
    - (lang dune 3.17)
    + (lang dune 3.19)

      ;; Atoms are ignored by dunolint (probably doesn't exists in dune).
      atom
    |}];
  Sexps_rewriter.reset (Dune_workspace_linter.sexps_rewriter t);
  (* There's a typed API to access the supported stanza. *)
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.match_stanza stanza with
    | Dune_workspace_linter.Dune_lang_version s ->
      (* Test the dune_lang_version stanza API and bump to [3.20]. *)
      print_s
        [%sexp
          (Dune_workspace_linter.Dune_lang_version.dune_lang_version s
           : Dune_workspace.Dune_lang_version.t)];
      [%expect {| 3.17 |}];
      Dune_workspace_linter.Dune_lang_version.set_dune_lang_version
        s
        ~dune_lang_version:(Dune_workspace.Dune_lang_version.create (3, 20));
      [%expect {||}];
      ()
    | _ -> ());
  print_diff t;
  [%expect
    {|
    --- expected
    +++ actual
    @@ -1,4 +1,4 @@
    - (lang dune 3.17)
    + (lang dune 3.20)

      ;; Atoms are ignored by dunolint (probably doesn't exists in dune).
      atom
    |}];
  (* You can also mix and match the typed API with the predicate language. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval; enforce = _ } ->
      (match
         eval
           ~path
           ~predicate:
             Dunolint.Config.Std.(
               `dune_workspace
                 (dune_lang_version
                    (eq (Dune_workspace.Dune_lang_version.create (3, 17)))))
       with
       | False | Undefined -> assert false
       | True ->
         let original_sexp = Dunolinter.original_sexp stanza in
         print_s original_sexp;
         [%expect {| (lang dune 3.17) |}];
         ());
      (* Test eval with path predicate. *)
      (match
         eval ~path ~predicate:Dunolint.Config.Std.(`path (glob "dune-workspace"))
       with
       | True -> print_s [%sexp "path matched"]
       | False | Undefined -> assert false);
      [%expect {| "path matched" |}]);
  (* You can also use the enforcement construct from the OCaml API. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval = _; enforce } ->
      let apply condition = enforce ~path ~condition in
      let () =
        let open Dunolint.Config.Std in
        apply
          (dune_workspace
             (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 5)))));
        (* Enforcing unapplicable invariants has no effect. *)
        apply (dune (library (name (equals (Dune.Library.Name.v "bar")))));
        apply
          (dunolint
             (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 0)))));
        apply (dune_project (name (equals (Dune_project.Name.v "foo"))));
        apply (path (glob "path/"));
        apply (not_ (dune (library (name (equals (Dune.Library.Name.v "bar"))))));
        apply
          (not_
             (dunolint
                (dunolint_lang_version
                   (eq (Dunolint0.Dunolint_lang_version.create (1, 0))))));
        apply (not_ (dune_project (name (equals (Dune_project.Name.v "foo")))))
      in
      ());
  print_diff t;
  [%expect
    {|
    --- expected
    +++ actual
    @@ -1,4 +1,4 @@
    - (lang dune 3.17)
    + (lang dune 4.5)

      ;; Atoms are ignored by dunolint (probably doesn't exists in dune).
      atom
    |}];
  ()
;;
