(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

let show_dune content =
  let content = String.trim content ^ "\n" in
  Out_channel.with_open_bin "dune" (fun oc -> output_string oc content);
  print_string content
;;

let lint_dune () =
  Printf.printf "$ dunolint tools lint-file dune\n";
  let ic = Unix.open_process_in "./dunolint.exe tools lint-file dune" in
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

   # Comments in `libraries`

   The `libraries` dependencies listed in `library` stanzas are sorted
   alphabetically. For example, this dune file: *)

let%expect_test "basic sorting" =
  show_dune
    {|
(library
 (name my_lib)
 (libraries foo bar baz))
    |};
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name my_lib)
     (libraries foo bar baz))
    |}];
  (* @mdexp is linted as follows: *)
  lint_dune ();
  (* @mdexp.snapshot { lang: "sh" } *)
  [%expect
    {|
    $ dunolint tools lint-file dune
    (library
     (name my_lib)
     (libraries bar baz foo))
    |}]
;;

(* @mdexp

   The process of reordering the libraries items becomes more complex in the
   presence of comments. This section illustrates how comments are handled in
   this field, discusses limitations, and suggests conventions to satisfy the
   linter in certain unsupported cases.

   ## End-of-line Comments

   When a comment is placed next to an entry, the tool assumes that it is
   attached to that item and moves it along with the item during reordering. *)

let%expect_test "end-of-line comments" =
  show_dune
    {|
(library
 (name my_lib)
 (libraries
  aa
  dd ;; this a comment
  zz
  bb
  cc))
    |};
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name my_lib)
     (libraries
      aa
      dd ;; this a comment
      zz
      bb
      cc))
    |}];
  lint_dune ();
  (* @mdexp.snapshot { lang: "sh" } *)
  [%expect
    {|
    $ dunolint tools lint-file dune
    (library
     (name my_lib)
     (libraries
      aa
      bb
      cc
      dd ;; this a comment
      zz))
    |}]
;;

(* @mdexp

   So far, so good.

   ## Sections Comments

   When a comment is placed on its own line, it is less clear whether it applies
   to the following line only or to multiple lines. The reordering implemented
   in *dunolint* assumes that comments on their own lines are *section delimiters*.
   Libraries within each section are reordered, but no reordering occurs
   between or across sections: *)

let%expect_test "sections comments" =
  show_dune
    {|
(library
 (name my_lib)
 (libraries
  ;; First section
  jj
  ii
  ;; Second section
  bb
  aa
  ;; Third section
  dd
  cc))
    |};
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name my_lib)
     (libraries
      ;; First section
      jj
      ii
      ;; Second section
      bb
      aa
      ;; Third section
      dd
      cc))
    |}];
  lint_dune ();
  (* @mdexp.snapshot { lang: "sh" } *)
  [%expect
    {|
    $ dunolint tools lint-file dune
    (library
     (name my_lib)
     (libraries
      ;; First section
      ii
      jj
      ;; Second section
      aa
      bb
      ;; Third section
      cc
      dd))
    |}]
;;

(* @mdexp

   ### Suggested Workaround

   This behavior may not produce the desired result when a comment is intended
   to apply only to the immediate subsequent line. For example: *)

let%expect_test "suggested workaround" =
  show_dune
    {|
(library
 (name my_lib)
 (libraries
  ;; foo needs to be first.
  foo
  baz
  bar))
    |};
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name my_lib)
     (libraries
      ;; foo needs to be first.
      foo
      baz
      bar))
    |}];
  (* @mdexp The linter processes this as follows: *)
  lint_dune ();
  (* @mdexp.snapshot { lang: "sh" } *)
  [%expect
    {|
    $ dunolint tools lint-file dune
    (library
     (name my_lib)
     (libraries
      ;; foo needs to be first.
      bar
      baz
      foo))
    |}];
  (* @mdexp

     To achieve the intended result, we recommend converting the comment into a
     section comment: *)
  show_dune
    {|
(library
 (name my_lib)
 (libraries
  ;; foo needs to be first.
  foo
  ;; Other libs may be ordered as usual.
  baz
  bar))
    |};
  (* @mdexp.snapshot { lang: "dune" } *)
  [%expect
    {|
    (library
     (name my_lib)
     (libraries
      ;; foo needs to be first.
      foo
      ;; Other libs may be ordered as usual.
      baz
      bar))
    |}];
  (* @mdexp

     The linter preserves the sections, ensuring the comment's scope is clear.
     This approach not only satisfies the linter but also makes the intent of
     the comment clearer to human readers: *)
  lint_dune ();
  (* @mdexp.snapshot { lang: "sh" } *)
  [%expect
    {|
    $ dunolint tools lint-file dune
    (library
     (name my_lib)
     (libraries
      ;; foo needs to be first.
      foo
      ;; Other libs may be ordered as usual.
      bar
      baz))
    |}]
;;

(* @mdexp

   By adopting this convention, you can maintain clarity and consistency in your
   code while working harmoniously with the linter. This approach helps you
   achieve a *linting equilibrium* — a balance where your code reflects both
   your intent and the linter's requirements, resulting in a more maintainable
   and readable codebase. *)
