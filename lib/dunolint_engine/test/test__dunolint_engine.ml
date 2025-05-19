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
module Unix = UnixLabels

let%expect_test "lint" =
  let t =
    Dunolint_engine.create ~config:(Dunolint_engine.Config.create ~running_mode:Dry_run)
  in
  Out_channel.write_all
    "dune-project"
    ~data:
      {|
(lang dune 3.17)

(name dunolint)

(implicit_transitive_deps true)

(generate_opam_files)
|};
  (* In this section we exercise some ways dunolint_engine can be used as a library. *)
  Dunolint_engine.lint_dune_project_file
    t
    ~path:(Relative_path.v "dune-project")
    ~f:(fun stanza ->
      (* The API has a few getters. *)
      print_s [%sexp (Dunolinter.path stanza : Relative_path.t)];
      [%expect {| dune-project |}];
      (* The intended use is to match on the actual stanza. *)
      (match Dunolinter.match_stanza stanza with
       | Dune_project_linter.Generate_opam_files _ ->
         (* You may explicitly ignore the ones you are not interested in. *)
         ()
       | Dune_project_linter.Implicit_transitive_deps s ->
         (* And use the typed getters and setters of the stanza you care about. *)
         print_s [%sexp (Dune_project_linter.Implicit_transitive_deps.value s : bool)];
         [%expect {| true |}];
         (* If you use setters, the side effect on the memory value is done
            right away, but actual sexp rewrite is going to be registered and only
            executed during the call to [materialize] (see below). *)
         Dune_project_linter.Implicit_transitive_deps.set_value s ~value:false;
         [%expect {||}];
         ()
       | Dune_project_linter.Name _ ->
         (* In this example we illustrate that it is also possible to use the eDSL API
            when using the dunolint engine as a library. For this purpose here,
            we are going to rewrite the name using an invariant. *)
         (match Dunolinter.linter stanza with
          | Unhandled -> assert false
          | T { eval = _; enforce } ->
            (* Similarly to using typed setters, the side effect is performed
               but the rewrite is done during materialization. *)
            enforce (dune_project (name (equals (Dune_project.Name.v "a-better-name"))));
            [%expect {||}];
            ())
       | _ ->
         (* Matching over values of extensible variant types must include a wild
            card pattern in order to be exhaustive.

            We make use of that design characteristic of OCaml to encourage the
            users to write future proof code, where new constructs can be added
            to the dunolint engine, and added over time to client codes. *)
         ());
      ())
    ~with_linter:(fun linter ->
      (* It is also possible to access the linter value that holds the stanzas
         and using it directly. Here we'll illustrate this use case with an
         example involving access to the low-level sexps-rewriter. *)
      print_s [%sexp (Dune_project_linter.path linter : Relative_path.t)];
      [%expect {| dune-project |}];
      let sexps_rewriter = Dune_project_linter.sexps_rewriter linter in
      Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
        match sexp with
        | Atom "3.17" ->
          File_rewriter.replace file_rewriter ~range ~text:"3.19";
          Break
        | _ -> Continue);
      ());
  Err.For_test.protect (fun () -> Dunolint_engine.materialize t);
  [%expect
    {|
    dry-run: Would edit file "dune-project":
    -1,8 +1,7
    +|(lang dune 3.19)

    -|(lang dune 3.17)
    +|(name a-better-name)

    -|(name dunolint)
    -|
    -|(implicit_transitive_deps true)
    +|(implicit_transitive_deps false)

      (generate_opam_files)
    |}];
  ()
;;

let%expect_test "format_dune_file" =
  let fmt =
    Dunolint_engine.format_dune_file
      ~new_contents:
        {|
(lang dune 3.17) (name dunolint)
|}
  in
  print_endline fmt;
  [%expect
    {|
    (lang dune 3.17)

    (name dunolint)
    |}];
  Err.For_test.protect (fun () ->
    match Dunolint_engine.format_dune_file ~new_contents:{|(invalid|} with
    | (_ : string) -> () [@coverage off]);
  [%expect
    {|
    Error: Failed to format dune file:
    <REDACTED IN TEST>
    Exited 1
    [123]
    |}];
  ()
;;

let%expect_test "create-files" =
  (* The engine may be used to create files where they are not initially present. *)
  let t =
    Dunolint_engine.create ~config:(Dunolint_engine.Config.create ~running_mode:Force_yes)
  in
  Dunolint_engine.lint_file t ~path:(Relative_path.v "lib/a/dune") ~create_file:(fun () ->
    let library = Dune_linter.Library.create ~name:(Dune.Library.Name.v "my-lib") () in
    Sexp.to_string_mach (Dune_linter.Library.write library));
  (* You can do several passes of linting before materializing. In this case
     the contents that is linted is the contents that is held in memory. *)
  (* If you do not supply a [rewrite_file] argument to [lint_file], existing
     files will stay untouched. *)
  Dunolint_engine.lint_file t ~path:(Relative_path.v "lib/a/dune");
  (* Another option to apply lints is to go through the [Dunolinter] API. *)
  Dunolint_engine.lint_dune_file t ~path:(Relative_path.v "lib/a/dune") ~f:(fun stanza ->
    match Dunolinter.linter stanza with
    | Unhandled ->
      (* The file was created above, and only contains stanzas supported by
         dunolint. Thus we are not exercising this [Unhandled] case during this
         test. In general, you want to ignore unsupported stanzas - they will
         not be linted and kept untouched. *)
      () [@coverage off]
    | T { eval = _; enforce } ->
      enforce
        (dune
           (library (public_name (equals (Dune.Library.Public_name.v "a-public-name")))));
      [%expect {||}];
      ());
  Err.For_test.protect (fun () -> Dunolint_engine.materialize t);
  [%expect
    {|
    Running `mkdir -p lib/a/`
    Editing file "lib/a/dune":
    -1,0 +1,3
    +|(library
    +| (name my-lib)
    +| (public_name a-public-name))
    |}];
  (* And the file indeed exists on disk after [materialize] has run. *)
  print_endline (In_channel.read_all "lib/a/dune");
  [%expect
    {|
    (library
     (name my-lib)
     (public_name a-public-name))
    |}];
  ()
;;

let%expect_test "lint-absent-files" =
  (* By default, [lint-file] will not create a file if no initializer is supplied. *)
  let t =
    Dunolint_engine.create ~config:(Dunolint_engine.Config.create ~running_mode:Force_yes)
  in
  Dunolint_engine.lint_file t ~path:(Relative_path.v "absent/file/dune");
  [%expect {||}];
  Err.For_test.protect (fun () -> Dunolint_engine.materialize t);
  [%expect {||}];
  print_s [%sexp (Stdlib.Sys.file_exists "absent/file/dune" : bool)];
  [%expect {| false |}]
;;

let%expect_test "mkdirs" =
  Err.For_test.protect (fun () ->
    Dunolint_engine.Private.mkdirs (Relative_path.v "path/to/directory");
    Out_channel.write_all "path/to/directory/file" ~data:"Hello\n";
    Dunolint_engine.Private.mkdirs (Relative_path.v "path/to/directory/file"));
  [%expect
    {|
    Error: Parent path "path/to/directory/file" is expected to be a directory.
    Actual file kind is [Regular file].
    [123]
    |}];
  ()
;;

let%expect_test "invalid files" =
  (* When encountering invalid files during linting, errors are reported, but
     the execution continues so other valid files are still linted. *)
  Err.For_test.protect (fun () ->
    let t =
      Dunolint_engine.create ~config:(Dunolint_engine.Config.create ~running_mode:Dry_run)
    in
    Unix.mkdir "invalid" ~perm:0o755;
    Out_channel.write_all
      "dune-project"
      ~data:
        {|
(lang dune 3.17)
|};
    Out_channel.write_all "invalid/dune" ~data:"(invalid";
    Out_channel.write_all "invalid/dune-project" ~data:"(invalid";
    Dunolint_engine.lint_dune_file
      t
      ~path:(Relative_path.v "invalid/dune")
      ~f:(ignore : Dune_linter.Stanza.t Dunolinter.Stanza.t -> unit);
    [%expect
      {|
      File "invalid/dune", line 1, characters 8-8:
      1 | (invalid

      Error: unclosed parentheses at end of input

      File "invalid/dune", line 1, characters 0-0:
      Error: Failed to format dune file:
      <REDACTED IN TEST>
      Exited 1
      |}];
    Dunolint_engine.lint_dune_project_file
      t
      ~path:(Relative_path.v "invalid/dune-project")
      ~f:(ignore : Dune_project_linter.Stanza.t Dunolinter.Stanza.t -> unit);
    [%expect
      {|
      File "invalid/dune-project", line 1, characters 8-8:
      1 | (invalid

      Error: unclosed parentheses at end of input

      File "invalid/dune-project", line 1, characters 0-0:
      Error: Failed to format dune file:
      <REDACTED IN TEST>
      Exited 1
      |}];
    Dunolint_engine.lint_dune_project_file
      t
      ~path:(Relative_path.v "dune-project")
      ~f:(ignore : Dune_project_linter.Stanza.t Dunolinter.Stanza.t -> unit);
    [%expect {||}];
    Dunolint_engine.materialize t;
    [%expect {| dry-run: Would edit file "dune-project": |}];
    ());
  [%expect {| [123] |}];
  ()
;;

let%expect_test "file errors" =
  Unix.mkdir "tempdir" ~perm:0o755;
  let t =
    Dunolint_engine.create ~config:(Dunolint_engine.Config.create ~running_mode:Force_yes)
  in
  (* If you are trying to lint a path that is not a regular file, you get an
     error right away rather than during [materialize]. *)
  Err.For_test.protect (fun () ->
    Dunolint_engine.lint_file t ~path:(Relative_path.v "tempdir"));
  [%expect
    {|
    Error: Linted file "tempdir" is expected to be a regular file.
    Actual file kind is [Directory].
    [123]
    |}];
  Err.For_test.protect (fun () -> Dunolint_engine.materialize t);
  [%expect {||}];
  ()
;;
