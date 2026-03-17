(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

(* This file tests the [Sections_handler.rewrite_sections] function, with a
   particular focus on the insertion behavior. *)

let field_name = "items"

let rewrite original_contents ~sections =
  let sexps_rewriter, field =
    Test_helpers.read_sexp_field ~path:(Fpath.v "dune") original_contents
  in
  Dunolinter.Sections_handler.rewrite_sections
    ~field_name
    ~sexps_rewriter
    ~field
    ~write_arg:Fn.id
    ~sections;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

(* Tests for the insertion behavior of [rewrite_sections]. *)

let%expect_test "insert - from empty field" =
  rewrite {|(items)|} ~sections:[ [ "foo" ] ];
  [%expect
    {|
    (items
    foo)
    |}];
  rewrite {|(items)|} ~sections:[ [ "foo"; "bar" ] ];
  [%expect
    {|
    (items
    foo
    bar)
    |}];
  ()
;;

let%expect_test "insert - appending to existing entries" =
  rewrite {|(items foo)|} ~sections:[ [ "foo"; "bar" ] ];
  [%expect
    {|
    (items foo
    bar)
    |}];
  rewrite {|(items foo bar)|} ~sections:[ [ "foo"; "bar"; "baz" ] ];
  [%expect
    {|
    (items foo bar
    baz)
    |}];
  rewrite
    {|
(items
 foo
 bar)
|}
    ~sections:[ [ "foo"; "bar"; "baz" ] ];
  [%expect
    {|
    (items
     foo
     bar
    baz)
    |}];
  ()
;;

let%expect_test "insert - with sections" =
  rewrite
    {|
(items
 ;; Section 1
 aa
 bb
 ;; Section 2
 cc
 dd)
|}
    ~sections:[ [ "aa"; "bb" ]; [ "cc"; "dd"; "ee" ] ];
  [%expect
    {|
    (items
     ;; Section 1
     aa
     bb
     ;; Section 2
     cc
     dd
    ee)
    |}];
  ()
;;

let%expect_test "insert - adding new sections" =
  rewrite
    {|
(items
 ;; Section 1
 aa
 bb)
|}
    ~sections:[ [ "aa"; "bb" ]; [ "cc"; "dd" ] ];
  [%expect
    {|
    (items
     ;; Section 1
     aa
     bb
    cc
    dd)
    |}];
  ()
;;

let%expect_test "replace and remove - preserve formatting" =
  rewrite {|(items foo bar baz)|} ~sections:[ [ "aaa"; "bbb"; "ccc" ] ];
  [%expect {| (items aaa bbb ccc) |}];
  rewrite {|(items foo bar baz)|} ~sections:[ [ "foo" ] ];
  [%expect {| (items foo  ) |}];
  rewrite {|(items foo bar)|} ~sections:[ [] ];
  [%expect {| (items  ) |}];
  ()
;;
