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
  Test_helpers.parse
    (module Dune_project_linter.Implicit_transitive_deps)
    ~path:(Fpath.v "dune-project")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_project_linter.Implicit_transitive_deps.write t))
  in
  test {| (implicit_transitive_deps true) |};
  [%expect {| (implicit_transitive_deps true) |}];
  test {| (implicit_transitive_deps false) |};
  [%expect {| (implicit_transitive_deps false) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune-project", line 1, characters 1-16:
    Error: Unexpected [Sexp] for field [implicit_transitive_deps].
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (implicit_transitive_deps true) |} in
  print_s [%sexp (t : Dune_project_linter.Implicit_transitive_deps.t)];
  [%expect {| ((value true)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_project_linter.Implicit_transitive_deps.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (implicit_transitive_deps true) |};
  [%expect {| (implicit_transitive_deps true) |}];
  rewrite {| (implicit_transitive_deps false) |};
  [%expect {| (implicit_transitive_deps false) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (implicit_transitive_deps true) |} ~f:(fun t ->
    print_s [%sexp (Dune_project_linter.Implicit_transitive_deps.value t : bool)];
    [%expect {| true |}];
    Dune_project_linter.Implicit_transitive_deps.set_value t ~value:false;
    print_s [%sexp (Dune_project_linter.Implicit_transitive_deps.value t : bool)];
    [%expect {| false |}];
    ());
  [%expect {| (implicit_transitive_deps false) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_project_linter.Implicit_transitive_deps.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_project_linter.Implicit_transitive_deps.create ~implicit_transitive_deps:true
  in
  test t {| (implicit_transitive_deps false) |};
  [%expect {| (implicit_transitive_deps true) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune_project.Implicit_transitive_deps.Predicate.t as 'a
    constraint 'a = [ `equals of bool ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (implicit_transitive_deps true) |} in
  Test_helpers.is_true
    (Dune_project_linter.Implicit_transitive_deps.eval t ~predicate:(`equals true));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_project_linter.Implicit_transitive_deps.eval t ~predicate:(`equals false));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Implicit_transitive_deps.enforce t ~condition);
      Dune_project_linter.Implicit_transitive_deps.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (implicit_transitive_deps true) |} in
  enforce t [];
  [%expect {| (implicit_transitive_deps true) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals true ];
  [%expect {| (implicit_transitive_deps true) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals false ];
  [%expect {| (implicit_transitive_deps false) |}];
  let t = parse {| (implicit_transitive_deps false) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (equals true) ];
  [%expect {| (implicit_transitive_deps false) |}];
  (* Enforcing the negation the equality equates enforcing its negated value. *)
  enforce t [ not_ (equals false) ];
  [%expect {| (implicit_transitive_deps true) |}];
  (* Blang. *)
  let t = parse {| (implicit_transitive_deps true) |} in
  enforce t [ true_ ];
  [%expect {| (implicit_transitive_deps true) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce t [ and_ [ not_ (equals false); equals true ] ];
  [%expect {| (implicit_transitive_deps true) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce t [ or_ [ equals true; equals false ] ];
  [%expect {| (implicit_transitive_deps true) |}];
  require_does_raise [%here] (fun () -> enforce t [ or_ [ equals false; equals false ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (equals false)
        (equals false))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant = if_ (equals true) (equals false) (equals true) in
  let t = parse {| (implicit_transitive_deps true) |} in
  enforce t [ invariant ];
  [%expect {| (implicit_transitive_deps false) |}];
  let t = parse {| (implicit_transitive_deps false) |} in
  enforce t [ invariant ];
  [%expect {| (implicit_transitive_deps true) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (implicit_transitive_deps true) |} in
  Test_helpers.is_true
    (Dune_project_linter.Implicit_transitive_deps.Linter.eval
       t
       ~predicate:(`implicit_transitive_deps (equals true)));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Implicit_transitive_deps.Linter.eval
       t
       ~predicate:(`generate_opam_files is_present));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Implicit_transitive_deps.Linter.eval t ~predicate:(`name true_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Implicit_transitive_deps.Linter.enforce t ~condition);
      Dune_project_linter.Implicit_transitive_deps.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (implicit_transitive_deps false) |} in
  enforce t [];
  [%expect {| (implicit_transitive_deps false) |}];
  enforce t [ implicit_transitive_deps (equals true) ];
  [%expect {| (implicit_transitive_deps true) |}];
  (* Enforcing other toplevel stanza has no effect. *)
  enforce t [ generate_opam_files is_present ];
  [%expect {| (implicit_transitive_deps true) |}];
  enforce t [ name false_ ];
  [%expect {| (implicit_transitive_deps true) |}];
  enforce t [ not_ (name false_) ];
  [%expect {| (implicit_transitive_deps true) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (implicit_transitive_deps true) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce t [ implicit_transitive_deps (not_ (equals true)) ];
  [%expect {| (implicit_transitive_deps false) |}];
  enforce
    t
    [ and_
        [ implicit_transitive_deps (equals false)
        ; implicit_transitive_deps (equals false)
        ]
    ];
  [%expect {| (implicit_transitive_deps false) |}];
  enforce
    t
    [ or_
        [ implicit_transitive_deps (equals true)
        ; implicit_transitive_deps (equals false)
        ]
    ];
  [%expect {| (implicit_transitive_deps false) |}];
  enforce
    t
    [ if_
        (implicit_transitive_deps (equals true))
        (implicit_transitive_deps (equals false))
        (implicit_transitive_deps (equals true))
    ];
  [%expect {| (implicit_transitive_deps true) |}];
  ()
;;
