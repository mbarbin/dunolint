(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* In these tests, the first element of each list and the atom [\] stay in place,
   and the other elements are sorted: atoms alphabetically, then lists in their
   original order. The elements of the lists starting with [keep] aren't reordered. *)

let compare (a : Sexp.t) (b : Sexp.t) : Ordering.t =
  match a, b with
  | Atom a, Atom b -> Ordering.of_int (String.compare a b)
  | Atom _, List _ -> Lt
  | List _, Atom _ -> Gt
  | List _, List _ -> Eq
;;

let recurse_into (elements : Sexp.t list) =
  match elements with
  | Atom "keep" :: _ -> false
  | _ -> true
;;

let is_fixed ~is_head (element : Sexp.t) =
  match element with
  | Atom "\\" -> true
  | Atom _ | List _ -> is_head
;;

let reorder_text original_contents =
  let sexps_rewriter, field =
    Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
  in
  Dunolinter.Reorder_sexp_in_place.reorder
    ~sexps_rewriter
    ~sexp:field
    ~recurse_into
    ~is_fixed
    ~compare
;;

(* Prints the reordered source, followed by its formatting by dune when it differs, to
   show the final result. *)
let reorder original_contents =
  let reordered = reorder_text original_contents in
  print_endline reordered;
  let formatted =
    Dunolint_engine.format_dune_file
      ~dune_lang_version:(Dune_project.Dune_lang_version.create (3, 17))
      ~new_contents:reordered
  in
  if not (String.equal formatted (reordered ^ "\n"))
  then (
    print_endline ";; Formatted:";
    print_string formatted)
;;

let%expect_test "flat" =
  reorder {| (items c a b) |};
  [%expect {| (items a b c) |}];
  reorder {| (items) |};
  [%expect {| (items) |}];
  (* The original spelling of the atoms is preserved. *)
  reorder {| (items "c" a b) |};
  [%expect {| (items a b "c") |}];
  ()
;;

let%expect_test "sections" =
  (* Comments placed on their own line and blank lines stay in place, and delimit
     sections. Note that the formatter removes the blank line, merging the last two
     sections, which would then be reordered together. *)
  reorder
    {|(items
 c
 a
 ; A section.
 z
 b

 y
 x)|};
  [%expect
    {|
    (items
     a
     c
     ; A section.
     b
     z

     x
     y)
    ;; Formatted:
    (items
     a
     c
     ; A section.
     b
     z
     x
     y)
    |}];
  ()
;;

let%expect_test "end of line comments" =
  (* An item moves along with the comment placed after it on the same line. *)
  reorder
    {|(items
 c ; About c.
 a
 b ; About b.
 )|};
  [%expect
    {|
    (items
     a
     b ; About b.
     c ; About c.
     )
    |}];
  (* When an item with a comment is moved before something on the same line, a newline
     is inserted after the comment. *)
  reorder
    {|(items
 c ; About c.
 a b)|};
  [%expect
    {|
    (items
     a
     b c ; About c.
     )
    ;; Formatted:
    (items
     a
     b
     c ; About c.
     )
    |}];
  reorder
    {|(items
 b ; About b.
 a(c))|};
  [%expect
    {|
    (items
     a
     b ; About b.
     (c))
    |}];
  (* The blanks that follow [a] at the end of its line stay in place, and end up after
     the comment of [b]. They don't count as something on the same line, thus no
     newline is inserted. The output is escaped to show them. *)
  print_dyn (Dyn.string (reorder_text "(items\n b ; About b.\n a \t\n c)"));
  [%expect
    {|
    "(items\n\
    \ a\n\
    \ b ; About b. \t\n\
    \ c)"
    |}];
  ()
;;

let%expect_test "block comments" =
  (* Block comments are part of the gaps, thus they stay in place. *)
  reorder {| (items c #| A block comment. |# a b) |};
  [%expect {| (items a #| A block comment. |# b c) |}];
  ()
;;

let%expect_test "nested lists" =
  (* Nested lists are reordered too, and move along with their contents. *)
  reorder {| (items z (or y x) a (not (and c b)) ()) |};
  [%expect
    {|
    (items a z (or x y) (not (and b c)) ())
    ;; Formatted:
    (items
     a
     z
     (or x y)
     (not
      (and b c))
     ())
    |}];
  (* The comments of a nested list follow the same rules. *)
  reorder
    {|(items
 (or
  y ; About y.
  x)
 a)|};
  [%expect
    {|
    (items
     a
     (or
      x
      y ; About y.
      ))
    |}];
  ()
;;

let%expect_test "recurse into" =
  (* The elements of a list that isn't recursed into are kept as in the source, but
     the list itself moves as an element of its parent. *)
  reorder {| (items c (keep z y) a (or (keep b a) d c)) |};
  [%expect
    {|
    (items a c (keep z y) (or c d (keep b a)))
    ;; Formatted:
    (items
     a
     c
     (keep z y)
     (or
      c
      d
      (keep b a)))
    |}];
  ()
;;

let%expect_test "fixed elements" =
  (* Elements are not moved across a fixed element. *)
  reorder {| (items d c \ b a) |};
  [%expect {| (items c d \ a b) |}];
  ()
;;
