(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* A test showing how to use the [Dune_workspace_linter] as a library. *)

let original_contents =
  {|
(lang dune 3.17)

;; [Dunolinter.linter] returns [Unhandled] for unhandled constructs.
(unhandled arg)

;; Atoms are ignored by dunolint (probably doesn't exists in dune).
atom
|}
  |> String.lstrip
;;

let print_diff t =
  let new_contents = Dune_workspace_linter.contents t in
  Myers.diff original_contents new_contents ~context:3 |> print_string
;;

let%expect_test "lint" =
  let path = Relative_path.v "dune-workspace" in
  let t =
    match Dune_workspace_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  print_diff t;
  [%expect {||}];
  print_dyn (Relative_path.to_dyn (Dune_workspace_linter.path t));
  [%expect {| "dune-workspace" |}];
  print_dyn (Dyn.int (List.length (Dune_workspace_linter.original_sexps t)));
  [%expect {| 3 |}];
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
    @@ -1,4 +1,4 @@
    -|(lang dune 3.17)
    +|(lang dune 3.19)

      ;; [Dunolinter.linter] returns [Unhandled] for unhandled constructs.
      (unhandled arg)
    |}];
  Sexps_rewriter.reset (Dune_workspace_linter.sexps_rewriter t);
  (* There's a typed API to access the supported stanza. *)
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.match_stanza stanza with
    | Dune_workspace_linter.Dune_lang_version s ->
      (* Test the dune_lang_version stanza API and bump to [3.20]. *)
      print_s
        (Dune_workspace_linter.Dune_lang_version.dune_lang_version s
         |> Dune_workspace.Dune_lang_version.sexp_of_t);
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
    @@ -1,4 +1,4 @@
    -|(lang dune 3.17)
    +|(lang dune 3.20)

      ;; [Dunolinter.linter] returns [Unhandled] for unhandled constructs.
      (unhandled arg)
    |}];
  (* You can also mix and match the typed API with the predicate language. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
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
       | True -> print_s (Atom "path matched")
       | False | Undefined -> assert false);
      [%expect {| "path matched" |}]);
  (* You can also use the enforcement construct from the OCaml API. *)
  Sexps_rewriter.reset sexps_rewriter;
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
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
    @@ -1,4 +1,4 @@
    -|(lang dune 3.17)
    +|(lang dune 4.5)

      ;; [Dunolinter.linter] returns [Unhandled] for unhandled constructs.
      (unhandled arg)
    |}];
  ()
;;

let%expect_test "eval selectors of other files" =
  let path = Relative_path.v "dune-workspace" in
  let t =
    match Dune_workspace_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  (* Selectors of other kinds of files evaluate to [Undefined]. *)
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
    | T { eval; enforce = _ } ->
      List.iter
        Dunolint.Config.Std.
          [ `dune (library (name (equals (Dune.Library.Name.v "foo"))))
          ; `dune_project (name (equals (Dune_project.Name.v "foo")))
          ; `dunolint
              (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 0))))
          ]
        ~f:(fun predicate -> Test_helpers.is_undefined (eval ~path ~predicate)));
  [%expect {||}];
  ()
;;

let%expect_test "enforce path" =
  let path = Relative_path.v "dune-workspace" in
  let t =
    match Dune_workspace_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
    | T { eval = _; enforce } ->
      let apply condition =
        Dunolinter.Handler.raise ~f:(fun () -> enforce ~path ~condition)
      in
      let open Dunolint.Config.Std in
      (* Enforcing [path] invariants that are satisfied has no effect. *)
      apply (path (glob "dune-workspace"));
      apply (not_ (path (glob "other/**")));
      [%expect {||}];
      (* The linter doesn't change the path of a file, thus enforcing an unsatisfied
         [path] invariant reports a failure. *)
      require_does_raise (fun () -> apply (path (glob "other/**")));
      [%expect
        {|
        (Dunolinter.Handler.Enforce_failure (loc _)
         (condition (path (glob other/**))))
        |}];
      require_does_raise (fun () -> apply (not_ (path (glob "dune-workspace"))));
      [%expect
        {|
        (Dunolinter.Handler.Enforce_failure (loc _)
         (condition (not (path (glob dune-workspace)))))
        |}];
      ());
  ()
;;

let%expect_test "enforce negated selector" =
  let path = Relative_path.v "dune-workspace" in
  let t =
    match Dune_workspace_linter.create ~path ~original_contents with
    | Ok t -> t
    | Error _ -> assert false
  in
  Dune_workspace_linter.visit t ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled -> ()
    | T { eval = _; enforce } ->
      let apply condition =
        Dunolinter.Handler.raise ~f:(fun () -> enforce ~path ~condition)
      in
      let open Dunolint.Config.Std in
      (* Enforcing a negated invariant that is satisfied has no effect. *)
      apply (not_ (dune_workspace false_));
      [%expect {||}];
      (* Enforcing an unsatisfied negated invariant that cannot be fixed reports a
         failure. *)
      require_does_raise (fun () -> apply (not_ (dune_workspace true_)));
      [%expect
        {|
        (Dunolinter.Handler.Enforce_failure (loc _)
         (condition (not (dune_workspace true))))
        |}];
      ());
  ()
;;
