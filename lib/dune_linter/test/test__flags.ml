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

let parse contents =
  Test_helpers.parse (module Dune_linter.Flags) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Flags.write t))
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
    Error: Unexpected [flags] field.
    [123]
    |}];
  test {| (flags) |};
  [%expect {| (flags) |}];
  (* In the most common cases, flags are atoms. *)
  test
    {|
(flags
  :standard
  -w
  +a-4-40-41-42-44-45-48-66
  -warn-error
  +a
  -open
  Foo)
|};
  [%expect {| (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Foo) |}];
  (* Dunolint also supports parsing more complex expressions. So far, we haven't
     restricted the use of nested sexps, and dunolint allows constructs that are
     invalid in dune, and not have any particular meaning. The aim is to be a
     bit more future-proof, however if this turns out to be a bad idea, this may
     be revisited later. Kept as characterization tests for now. *)
  test {| (flags (sexp bar)) |};
  [%expect {| (flags (sexp bar)) |}];
  ()
;;

let%expect_test "sexp_of" =
  let test str =
    let _, t = parse str in
    print_s [%sexp (t : Dune_linter.Flags.t)]
  in
  test
    {|
(flags
  :standard
  -w
  +a-4-40-41-42-44-45-48-66
  -warn-error
  +a
  -open
  Foo)
|};
  [%expect
    {| ((flags (:standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Foo))) |}];
  ()
;;

(* At the moment there is no predicate nor enforceable conditions on flags.
   We'll revisit when we add some. *)

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Flags.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (flags) |};
  [%expect {| (flags) |}];
  rewrite {| (flags :standard -open Foo) |};
  [%expect {| (flags :standard -open Foo) |}];
  (* Here we exercise the getters. *)
  rewrite {| (flags) |} ~f:(fun t ->
    print_s [%sexp (Dune_linter.Flags.is_empty t : bool)];
    [%expect {| true |}];
    print_s [%sexp (Dune_linter.Flags.flags t : Sexp.t list)];
    [%expect {| () |}];
    ());
  [%expect {| (flags) |}];
  rewrite {| (flags :standard -open Foo) |} ~f:(fun t ->
    print_s [%sexp (Dune_linter.Flags.is_empty t : bool)];
    [%expect {| false |}];
    print_s [%sexp (Dune_linter.Flags.flags t : Sexp.t list)];
    [%expect {| (:standard -open Foo) |}];
    ());
  [%expect {| (flags :standard -open Foo) |}];
  (* Exercising some setters. *)
  rewrite {| (flags :standard -open Foo) |} ~f:(fun t ->
    Dune_linter.Flags.set_flags t ~flags:[ [%sexp "foo"]; [%sexp Some "bar"] ];
    ());
  (* At the moment the flags rewrite does not remove flags that are already
     present on disk at the end of the rewritten position. That is a leftover
     from another system, and should be revisited. *)
  [%expect {| (flags foo (Some bar) Foo) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Flags.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Flags.create ~flags:[] in
  test t {| (flags) |};
  [%expect {| (flags) |}];
  test t {| (flags foo bar) |};
  [%expect {| (flags foo bar) |}];
  let t = Dune_linter.Flags.create ~flags:[ Sexp.Atom ":standard" ] in
  test t {| (flags) |};
  [%expect {| (flags :standard) |}];
  test t {| (flags foo bar) |};
  [%expect {| (flags :standard bar) |}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Flags.enforce t ~condition);
      Dune_linter.Flags.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (flags :standard -open Foo) |} in
  enforce t [];
  [%expect {| (flags :standard -open Foo) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (flags :standard -open Foo) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  ()
;;

(* See the cram test ["unexpected-merlin-flags-rewrite.t"] in this repository.
   Here we monitor the behavior of the rewrite of the flag section, when
   exercised in isolation, as opposed to within a [library] stanza. *)
let%expect_test "unexpected-rewrite-of-flag" =
  rewrite
    {|
(flags
 :standard
 -open Ocaml_utils
 -open Ocaml_parsing
 -open Ocaml_preprocess
 -open Ocaml_typing
 -open Ocaml_preprocess
 -open Merlin_utils)
|};
  [%expect
    {|
    (flags
     :standard
     -open Ocaml_utils
     -open Ocaml_parsing
     -open Ocaml_preprocess
     -open Ocaml_typing
     -open Ocaml_preprocess
     -open Merlin_utils)
    |}];
  ()
;;
