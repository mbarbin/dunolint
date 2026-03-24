(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let write_file path content =
  let content = String.trim content ^ "\n" in
  let dir = Filename.dirname path in
  if dir <> "." && not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  Out_channel.with_open_bin path (fun oc -> output_string oc content)
;;

let print_file path =
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic;
  print_string content
;;

let dunolint args =
  let display = "dunolint " ^ String.concat " " args in
  Printf.printf "$ %s\n" display;
  let cmd = "./dunolint.exe " ^ String.concat " " args in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  print_string output;
  match status with
  | WEXITED 0 -> ()
  | _ ->
    (match[@coverage off] status with
     | WEXITED n -> Printf.printf "[%d]\n" n
     | WSIGNALED n -> Printf.printf "[signal %d]\n" n
     | WSTOPPED n -> Printf.printf "[stopped %d]\n" n)
;;

(* @mdexp

   # Quick Start

   This is your first introduction to dunolint — a tool that helps you maintain
   consistent build configurations across OCaml projects. If you're familiar
   with dune but new to dunolint, this tutorial will show you the core concepts
   in under a minute.

   ## A Simple Library

   Consider a project with a simple library: *)

let%expect_test "quick start" =
  Out_channel.with_open_bin "dune-workspace" (fun _ -> ());
  write_file
    "src/dune"
    {|
(library
 (name mylib))
    |};
  (* @mdexp `src/dune`: *)
  print_file "src/dune";
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name mylib))
    |}];
  (* @mdexp

     ## Creating Your First Dunolint Configuration

     Say you want all libraries & executables to have code coverage
     instrumentation.

     Create a config file named `dunolint`: *)
  write_file
    "dunolint"
    {|
(lang dunolint 1.0)

(rule
 (enforce (dune (instrumentation (backend bisect_ppx)))))
    |};
  (* @mdexp `dunolint`: *)
  print_file "dunolint";
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (lang dunolint 1.0)

    (rule
     (enforce (dune (instrumentation (backend bisect_ppx)))))
    |}];
  (* @mdexp

     ## Seeing Dunolint in Action

     Check what needs to be fixed: *)
  dunolint [ "lint"; "--dry-run" ];
  (* @mdexp.snapshot { lang: "diff" } *)
  [%expect
    {|
    $ dunolint lint --dry-run
    dry-run: Would edit file "src/dune":
    @@ -1,2 +1,4 @@
      (library
    -| (name mylib))
    +| (name mylib)
    +| (instrumentation
    +|  (backend bisect_ppx)))
    |}];
  (* @mdexp Apply the fix: *)
  dunolint [ "lint"; "--yes" ];
  (* @mdexp.snapshot { lang: "diff" } *)
  [%expect
    {|
    $ dunolint lint --yes
    Editing file "src/dune":
    @@ -1,2 +1,4 @@
      (library
    -| (name mylib))
    +| (name mylib)
    +| (instrumentation
    +|  (backend bisect_ppx)))
    |}]
;;

(* @mdexp

   That's it! In under a minute, you've seen how dunolint enforces consistent
   build configurations across your entire project — both existing code and
   anything you add later.

   ## Next Steps

   - [Install dunolint](../../guides/installation) in your project
   - Learn the [configuration language](../../reference/config/README.md)
   - Understand dunolint's [design principles](../../explanation/README.md) *)
