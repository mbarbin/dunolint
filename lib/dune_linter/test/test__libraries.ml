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

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let sexps_rewriter, field = Common.read contents in
      let t = Dune_linter.Libraries.read ~sexps_rewriter ~field in
      print_s (Dune_linter.Libraries.write t))
  in
  test {||};
  [%expect
    {|
    Error: Expected exactly 1 sexp, got 0.
    [123]
    |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [libraries] field.
    [123]
    |}];
  test {| (libraries) |};
  [%expect {| (libraries) |}];
  (* In the most common case, dependencies are specified as atoms. *)
  test {| (libraries foo bar baz) |};
  [%expect {| (libraries foo bar baz) |}];
  (* Dunolint also support parsing more complex expressions. So far, we haven't
     restricted the use of composite sexps, and dunolint allows constructs that
     are invalid in dune, and not have any particular meaning. The aim is to be
     a bit more future-proof, however if this turns out to be a bad idea, this
     may be revisited later. Kept as characterization tests for now. *)
  test {| (libraries (re_export foo) bar (sexp baz)) |};
  [%expect {| (libraries (re_export foo) bar (sexp baz)) |}];
  ()
;;

let%expect_test "sexp_of" =
  let test str =
    let sexps_rewriter, field = Common.read str in
    let t = Dune_linter.Libraries.read ~sexps_rewriter ~field in
    print_s [%sexp (t : Dune_linter.Libraries.t)]
  in
  test
    {|
(libraries
 ;; Hey this is a comment in its own line.
 foo ;; this is a comment for foo.
 bar
 (re_export baz)
 sna
 (invalid sexp))
|};
  [%expect
    {|
    ((
      sections (((
        entries (
          (Library
            (name   foo)
            (source "foo ;; this is a comment for foo."))
          (Library   (name bar) (source bar))
          (Re_export (name baz) (source "(re_export baz)"))
          (Library   (name sna) (source sna))
          (Unhandled
            (original_index 4)
            (sexp (invalid sexp))
            (source "(invalid sexp)"))))))))
    |}];
  (* Entries separated by more than one line are treated as belonging to
     different sections. *)
  test
    {|
(libraries
 foo ;; this is a comment for foo.

 bar
 (re_export baz)
 sna)
|};
  [%expect
    {|
    ((
      sections (
        ((
          entries ((
            Library
            (name   foo)
            (source "foo ;; this is a comment for foo.")))))
        ((
          entries (
            (Library   (name bar) (source bar))
            (Re_export (name baz) (source "(re_export baz)"))
            (Library   (name sna) (source sna))))))))
    |}];
  (* In particular this can be achieved with a style where sections are
     separated by comments. *)
  test
    {|
(libraries
 ;; First section
 jj
 ii
 ;; Section section
 bb
 aa
 ;; Third section
 dd
 cc)
|};
  [%expect
    {|
    ((
      sections (
        ((
          entries (
            (Library (name jj) (source jj))
            (Library (name ii) (source ii)))))
        ((
          entries (
            (Library (name bb) (source bb))
            (Library (name aa) (source aa)))))
        ((
          entries (
            (Library (name dd) (source dd))
            (Library (name cc) (source cc))))))))
    |}];
  ()
;;

(* At the moment there is no predicate nor enforceable conditions on libraries.
   We'll revisit when we add some. *)

let%expect_test "rewrite" =
  let test ?(f = ignore) str =
    let sexps_rewriter, field = Common.read str in
    let t = Dune_linter.Libraries.read ~sexps_rewriter ~field in
    f t;
    Dune_linter.Libraries.rewrite t ~sexps_rewriter ~field;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  test {| (libraries) |};
  [%expect {| (libraries) |}];
  test {| (libraries foo bar baz) |};
  [%expect {| (libraries foo bar baz) |}];
  (* Here we exercise the getters. *)
  test
    {|
(libraries
 ;; Hey this is a comment in its own line.
 foo ;; this is a comment for foo.
 bar
 (re_export baz)
 sna
 (invalid sexp))
|}
    ~f:(fun t ->
      print_s [%sexp (Dune_linter.Libraries.is_empty t : bool)];
      [%expect {| false |}];
      print_s
        [%sexp (Dune_linter.Libraries.entries t : Dune_linter.Libraries.Entry.t list)];
      [%expect
        {|
        ((Library
           (name   foo)
           (source "foo ;; this is a comment for foo."))
         (Library   (name bar) (source bar))
         (Re_export (name baz) (source "(re_export baz)"))
         (Library   (name sna) (source sna))
         (Unhandled
           (original_index 4)
           (sexp (invalid sexp))
           (source "(invalid sexp)")))
        |}];
      let mem name = Dune_linter.Libraries.mem t ~library:(Dune.Library.Name.v name) in
      require [%here] (mem "foo");
      [%expect {||}];
      require [%here] (not (mem "not-here"));
      [%expect {||}];
      (* Re-exported libraries are recognized as members. *)
      require [%here] (mem "baz");
      [%expect {||}];
      ());
  [%expect
    {|
    (libraries
     ;; Hey this is a comment in its own line.
     foo ;; this is a comment for foo.
     bar
     (re_export baz)
     sna
     (invalid sexp))
    |}];
  (* Let's now exercise some setters. The tests on the sorting are left out of
     this section, and are written in a subsequent part below. *)
  test
    {|
(libraries
 foo ;; this is a comment for foo.
 (re_export baz))
|}
    ~f:(fun t ->
      Dune_linter.Libraries.add_libraries t ~libraries:[ Dune.Library.Name.v "bar" ];
      Dune_linter.Libraries.add_entries
        t
        ~entries:[ Dune_linter.Libraries.Entry.re_export (Dune.Library.Name.v "sna") ];
      ());
  [%expect
    {|
    (libraries
     foo ;; this is a comment for foo.
     (re_export baz)
    bar
    (re_export sna))
    |}];
  (* Adding library shall also work when starting from the empty set. *)
  test {| (libraries) |} ~f:(fun t ->
    Dune_linter.Libraries.add_libraries
      t
      ~libraries:[ Dune.Library.Name.v "foo"; Dune.Library.Name.v "bar" ];
    ());
  [%expect
    {|
     (libraries
    foo
    bar)
    |}];
  ()
;;

let%expect_test "sort" =
  let test str =
    let sexps_rewriter, field = Common.read str in
    let t = Dune_linter.Libraries.read ~sexps_rewriter ~field in
    Dune_linter.Libraries.dedup_and_sort t;
    Dune_linter.Libraries.rewrite t ~sexps_rewriter ~field;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  test {| (libraries) |};
  [%expect {| (libraries) |}];
  (* The most simple use case is to simply sort the library names
     alphabetically. *)
  test {| (libraries foo bar baz) |};
  [%expect {| (libraries bar baz foo) |}];
  (* The sorting respect the formatting, reads the name from the [re_export]
     construct, and keeps the comments attached to a library when it is on the
     same line. *)
  test
    {|
(libraries
 foo ;; this is a comment for foo.
 (re_export baz)
 bar)
|};
  [%expect
    {|
    (libraries
     bar
     (re_export baz)
     foo ;; this is a comment for foo.)
    |}];
  (* Handling of comments when they are in their own lines.

     When libraries are separated by more than one line, dunolint treats the
     dependencies as belonging to different sections, and sort each section in
     isolation. *)
  test
    {|
 (libraries
  dd
  aa
  ;; this a comment
  zz
  bb
  cc)
|};
  [%expect
    {|
    (libraries
     aa
     dd
     ;; this a comment
     bb
     cc
     zz)
    |}];
  test
    {|
(libraries
 ;; First section
 jj
 ii
 ;; Section section
 bb
 aa
 ;; Third section
 dd
 cc)
|};
  [%expect
    {|
    (libraries
     ;; First section
     ii
     jj
     ;; Section section
     aa
     bb
     ;; Third section
     cc
     dd)
    |}];
  (* If a comment is meant to target a single library, and that library is meant
     to be excluded from the sorting, it is possible to add an extra comment
     after it to create a new section for the remaining values.

     For example, consider this use-case, which will be incorrectly sorted by
     dunolint as it is: *)
  test
    {|
 (libraries
  ;; foo needs to be first
  foo
  bar
  baz)
|};
  [%expect
    {|
    (libraries
     ;; foo needs to be first
     bar
     baz
     foo)
    |}];
  (* It may be rewritten as follows *)
  test
    {|
 (libraries
  ;; foo needs to be first
  foo
  ;; The rest can be sorted as usual
  bar
  baz)
|};
  [%expect
    {|
    (libraries
     ;; foo needs to be first
     foo
     ;; The rest can be sorted as usual
     bar
     baz)
    |}];
  (* There may be cases where dunolint's sorting behavior will not make the most
     sense. We may further revisit at a later point. We keep these cases as
     characterization tests for now.

     See also: https://github.com/mbarbin/dunolint/issues/12 *)
  ()
;;

let%expect_test "dedup" =
  let test str =
    let sexps_rewriter, field = Common.read str in
    let t = Dune_linter.Libraries.read ~sexps_rewriter ~field in
    Dune_linter.Libraries.dedup_and_sort t;
    Dune_linter.Libraries.rewrite t ~sexps_rewriter ~field;
    print_endline (Sexps_rewriter.contents sexps_rewriter)
  in
  (* The sorting does also perform a deduping of the entries.

     Note that the space separated are not properly removed from the output.
     This is due to the fact that we do not normally make formatting efforts in
     dunolint itself, because we'd like to encourage dunolint users to enable
     dune auto formatting of dune files. If this proves too big of a hurdle,
     maybe this could be revisited on a case-by-case basis (TBD). *)
  test {| (libraries foo bar baz foo foo bar) |};
  [%expect {| (libraries bar baz foo   ) |}];
  (* When the dependencies are into different sections, the deduping should
     still remove the duplicates. *)
  test
    {|
(libraries
  ;; First section
  foo
  bar
  ;; Section section
  baz
  foo
  ;; Third section
  sna foo)
|};
  [%expect
    {|
    (libraries
      ;; First section
      bar
      foo
      ;; Section section
      baz

      ;; Third section
      sna )
    |}];
  ()
;;
