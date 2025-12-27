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
(lang dune 3.17)

(name dunolint)

(implicit_transitive_deps true)

(generate_opam_files)

;; Atoms are ignored by dunolint (probably doesn't exists in dune).
atom
|}
  |> String.lstrip
;;

let print_diff t =
  let new_contents = Dune_project_linter.contents t in
  Expect_test_patdiff.print_patdiff original_contents new_contents ~context:3
;;

let%expect_test "lint" =
  let path = Relative_path.v "path/to/dune-project" in
  let t =
    match Dune_project_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  print_diff t;
  [%expect {||}];
  print_s [%sexp (Dune_project_linter.path t : Relative_path.t)];
  [%expect {| path/to/dune-project |}];
  (* We can use the low-level sexps-rewriter API if we wish. *)
  let sexps_rewriter = Dune_project_linter.sexps_rewriter t in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "3.17" ->
      File_rewriter.replace file_rewriter ~range ~text:"3.19";
      Break
    | _ -> Continue);
  print_diff t;
  [%expect
    {|
    -1,4 +1,4
    -|(lang dune 3.17)
    +|(lang dune 3.19)

      (name dunolint)
    |}];
  Sexps_rewriter.reset (Dune_project_linter.sexps_rewriter t);
  (* There's a typed API to access the supported stanza. *)
  Dune_project_linter.visit t ~f:(fun stanza ->
    match Dunolinter.match_stanza stanza with
    | Dune_project_linter.Implicit_transitive_deps s ->
      (* And use the typed getters and setters of the stanza you care about. *)
      print_s
        [%sexp
          (Dune_project_linter.Implicit_transitive_deps.value s
           : Dune_project_linter.Implicit_transitive_deps.Value.t)];
      [%expect {| true |}];
      (* If you use setters, the side effect on the memory value is done right
         away, but actual sexp rewrite is going to be registered and only
         executed during the call to [materialize] (see below). *)
      Dune_project_linter.Implicit_transitive_deps.set_value s ~value:`False;
      [%expect {||}];
      ()
    | Dune_project_linter.Dune_lang_version s ->
      (* Test the dune_lang_version stanza API and bump to [3.20]. *)
      print_s
        [%sexp
          (Dune_project_linter.Dune_lang_version.dune_lang_version s
           : Dune_project.Dune_lang_version.t)];
      [%expect {| 3.17 |}];
      Dune_project_linter.Dune_lang_version.set_dune_lang_version
        s
        ~dune_lang_version:(Dune_project.Dune_lang_version.create (3, 20));
      [%expect {||}];
      ()
    | _ -> ());
  print_diff t;
  [%expect
    {|
    -1,8 +1,8
    -|(lang dune 3.17)
    +|(lang dune 3.20)

      (name dunolint)

    -|(implicit_transitive_deps true)
    +|(implicit_transitive_deps false)

      (generate_opam_files)
    |}];
  (* You can also mix and match the typed API with the predicate language. *)
  Dune_project_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval; enforce = _ } ->
      (match
         eval
           ~path
           ~predicate:
             Dunolint.Config.Std.(`dune_project (generate_opam_files Blang.true_))
       with
       | False -> assert false
       | Undefined -> ()
       | True ->
         let original_sexp = Dunolinter.original_sexp stanza in
         print_s original_sexp;
         [%expect {| (generate_opam_files) |}];
         let sexps_rewriter = Dunolinter.sexps_rewriter stanza in
         let { Loc.Range.start; stop } =
           Sexps_rewriter.range sexps_rewriter original_sexp
         in
         File_rewriter.remove
           (Sexps_rewriter.file_rewriter sexps_rewriter)
           ~range:{ start; stop = stop + 1 };
         ()));
  print_diff t;
  [%expect
    {|
    -1,10 +1,9
    -|(lang dune 3.17)
    +|(lang dune 3.20)

      (name dunolint)

    -|(implicit_transitive_deps true)
    +|(implicit_transitive_deps false)

    -|(generate_opam_files)

      ;; Atoms are ignored by dunolint (probably doesn't exists in dune).
      atom
    |}];
  (* You can also use the enforcement construct from the OCaml API. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_project_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> () [@coverage off]
    | T { eval = _; enforce } ->
      let apply condition = enforce ~path ~condition in
      let () =
        let open Dunolint.Config.Std in
        apply (dune_project (name (equals (Dune_project.Name.v "foo"))));
        (* Enforcing unapplicable invariants has no effect. *)
        apply (dune (library (name (equals (Dune.Library.Name.v "bar")))));
        apply
          (dunolint
             (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 0)))));
        apply (path (glob "path/"));
        apply (not_ (dune (library (name (equals (Dune.Library.Name.v "bar"))))));
        apply
          (not_
             (dunolint
                (dunolint_lang_version
                   (eq (Dunolint0.Dunolint_lang_version.create (1, 0))))))
      in
      (* Enforcing invariant that cannot be auto-corrected triggers an error. *)
      (match Dunolinter.match_stanza stanza with
       | Dune_project_linter.Name _ ->
         let open Dunolint.Config.Std in
         require_does_raise [%here] (fun () ->
           Dunolinter.Handler.raise ~f:(fun () ->
             apply (dune_project (name (not_ (equals (Dune_project.Name.v "foo")))))));
         [%expect
           {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals foo)))) |}];
         ()
       | _ -> ());
      ());
  print_diff t;
  [%expect
    {|
    -1,6 +1,6
      (lang dune 3.17)

    -|(name dunolint)
    +|(name foo)

      (implicit_transitive_deps true)
    |}];
  ()
;;
