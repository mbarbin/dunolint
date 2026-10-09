(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* The predicates are read from the arguments of an [items] field. *)
let read original_contents =
  Err.For_test.protect (fun () ->
    let sexps_rewriter, field =
      Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
    in
    let args =
      Dunolinter.Sexp_handler.get_args ~field_name:"items" ~sexps_rewriter ~field
    in
    let t =
      Dunolinter.Predicate_lang.read
        ~read_element:(fun ~sexps_rewriter:_ sexp -> sexp)
        ~sexps_rewriter
        args
    in
    print_s (Dunolinter.Predicate_lang.sexp_of_t Fun.id t))
;;

let%expect_test "read" =
  read {| (items foo bar*) |};
  [%expect {| (or (element foo) (element bar*)) |}];
  read {| (items) |};
  [%expect {| (or) |}];
  read {| (items :standard \ foo bar) |};
  [%expect {| (diff (or :standard) (or (element foo) (element bar))) |}];
  (* The operands following [\] are read as a sequence too. *)
  read {| (items a \ b \ c) |};
  [%expect {| (diff (or (element a)) (diff (or (element b)) (or (element c)))) |}];
  read {| (items (or a b) (and c (not d e)) (:standard g) (-flag)) |};
  [%expect
    {|
    (or (or (element a) (element b))
     (and (element c) (not (or (element d) (element e))))
     (or :standard (element g)) (or (element -flag)))
    |}];
  (* Quoted atoms are elements, even when they spell an operator or a symbol. *)
  read {| (items ("or" a) ":standard" "\\") |};
  [%expect {| (or (or (element or) (element a)) (element :standard) (element "\\")) |}];
  read {| (items (and a b \ c)) |};
  [%expect {| (or (diff (and (element a) (element b)) (or (element c)))) |}];
  (* A difference may have an empty left side, as dune allows it. An empty [and] is
     true, thus [(and \ b)] holds for any name but [b], whereas an empty [or] is false,
     thus [(or \ b)] holds for no name. A list starting with [\] isn't valid though:
     like any unquoted atom other than an operator, dune reserves it, thus [(\ b)] is
     read as an unknown construct. *)
  read {| (items (and \ b) (or \ b) (\ b)) |};
  [%expect
    {|
    (or (diff (and) (or (element b))) (diff (or) (or (element b)))
     (unknown ("\\" b)))
    |}];
  ()
;;

let%expect_test "unknown constructs" =
  (* The constructs dunolint doesn't know are kept as they are: symbols other than
     [:standard], [(:include ...)], and the lists whose head is an unquoted atom other
     than an operator, which dune reserves for future constructs. *)
  read {| (items :other (:include file) (if a b)) |};
  [%expect {| (or (unknown :other) (unknown (:include file)) (unknown (if a b))) |}];
  (* A list whose head is a quoted atom, or starts with [-] or [:], is a union. *)
  read {| (items ("foo" bar) (-x y) (:standard z)) |};
  [%expect
    {|
    (or (or (element foo) (element bar)) (or (element -x) (element y))
     (or :standard (element z)))
    |}];
  ()
;;

let canonical_sort_field original_contents =
  let sexps_rewriter, field =
    Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
  in
  print_endline (Dunolinter.Predicate_lang.canonical_sort_field ~sexps_rewriter ~field)
;;

let%expect_test "canonical_sort_field" =
  (* The arguments of the field are the operands of a union. *)
  canonical_sort_field {| (items c a :standard b) |};
  [%expect {| (items :standard a b c) |}];
  (* A field that is an atom is left as is. *)
  canonical_sort_field {| items |};
  [%expect {| items |}];
  (* The operands of [or], [and] and [not] commute: [:standard] comes first, then the
     elements in alphabetical order, then the compound operands (see below). *)
  canonical_sort_field {| (items (or c (and z y) :standard a (not x w) b :standard)) |};
  [%expect {| (items (or :standard :standard a b c (and y z) (not w x))) |}];
  (* An operator stays in place only at the head of its list, elsewhere it's an
     element. *)
  canonical_sort_field {| (items (or c and (and b a))) |};
  [%expect {| (items (or and c (and a b))) |}];
  (* A list without operator is a union, whose operands commute too, except for the
     first one: dune requires it to be quoted (unless it starts with [-] or [:]), and
     the quoting of the operands is not changed. *)
  canonical_sort_field {| (items (or ("c" b a) (:standard b a :standard))) |};
  [%expect {| (items (or (:standard :standard a b) ("c" a b))) |}];
  (* The elements of unknown constructs aren't reordered, but the constructs move as
     operands. *)
  canonical_sort_field {| (items (or c (if b a) ("x" z y) a)) |};
  [%expect {| (items (or a c ("x" y z) (if b a))) |}];
  (* Compound operands come after the atoms: the unions, then [or], [and] and [not].
     Operands of the same kind are compared by their sorted operands, and the unknown
     constructs come last, in their original order. *)
  canonical_sort_field
    {| (items (not b) (if 2) (and y x) (or q p) :standard ("b" a) a (if 1)) |};
  [%expect {| (items :standard a ("b" a) (or p q) (and x y) (not b) (if 2) (if 1)) |}];
  (* The comparison doesn't depend on the order of the operands of the compound
     operands, thus sorting again leaves the result unchanged. *)
  canonical_sort_field {| (items (or c a) (or b a)) |};
  [%expect {| (items (or a b) (or a c)) |}];
  canonical_sort_field {| (items (or a b) (or a c)) |};
  [%expect {| (items (or a b) (or a c)) |}];
  (* Shorter lists of operands come first, and nested compound operands are compared
     the same way. *)
  canonical_sort_field {| (items (or a) (or a b) (or a (or c) b) (or a ("b" c))) |};
  [%expect {| (items (or a) (or a b) (or a b (or c)) (or a ("b" c))) |}];
  (* The two sides of a difference are compared in turn. *)
  canonical_sort_field {| (items (and a \ c) (and a \ b) (and a)) |};
  [%expect {| (items (and a) (and a \ b) (and a \ c)) |}];
  canonical_sort_field {| (items (and b) (and a \ b) (and \ b)) |};
  [%expect {| (items (and \ b) (and a \ b) (and b)) |}];
  (* The operands are not moved across [\]. *)
  canonical_sort_field {| (items (and d c \ b a)) |};
  [%expect {| (items (and c d \ a b)) |}];
  (* Comments placed on their own line delimit sections. *)
  canonical_sort_field
    {|(items
 c
 a
 ; A section.
 b
 a)|};
  [%expect
    {|
    (items
     a
     c
     ; A section.
     a
     b)
    |}];
  ()
;;
