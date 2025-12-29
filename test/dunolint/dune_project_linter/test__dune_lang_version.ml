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
    (module Dune_project_linter.Dune_lang_version)
    ~path:(Fpath.v "dune-project")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_project_linter.Dune_lang_version.write t))
  in
  test {| (lang dune 3.20) |};
  [%expect {| (lang dune 3.20) |}];
  test {| (lang dune 4.5) |};
  [%expect {| (lang dune 4.5) |}];
  test {| (lang dune 3.INVALID) |};
  [%expect
    {|
    File "dune-project", line 1, characters 12-21:
    Error: Invalid version format: "3.INVALID".
    [123]
    |}];
  test {| (lang dune invalid) |};
  [%expect
    {|
    File "dune-project", line 1, characters 12-19:
    Error: Expected VERSION.MINOR format, got: "invalid".
    [123]
    |}];
  test {| (lang invalid 3.20) |};
  [%expect
    {|
    File "dune-project", line 1, characters 7-14:
    Error: Expected (lang dune VERSION) format.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (lang dune 3.20) |} in
  print_s [%sexp (t : Dune_project_linter.Dune_lang_version.t)];
  [%expect {| ((dune_lang_version 3.20)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_project_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (lang dune 3.20) |};
  [%expect {| (lang dune 3.20) |}];
  rewrite {| (lang dune 4.5) |};
  [%expect {| (lang dune 4.5) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (lang dune 3.20) |} ~f:(fun t ->
    print_s
      [%sexp
        (Dune_project_linter.Dune_lang_version.dune_lang_version t
         : Dune_project.Dune_lang_version.t)];
    [%expect {| 3.20 |}];
    Dune_project_linter.Dune_lang_version.set_dune_lang_version
      t
      ~dune_lang_version:(Dune_project.Dune_lang_version.create (4, 10));
    print_s
      [%sexp
        (Dune_project_linter.Dune_lang_version.dune_lang_version t
         : Dune_project.Dune_lang_version.t)];
    [%expect {| 4.10 |}];
    ());
  [%expect {| (lang dune 4.10) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_project_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_project_linter.Dune_lang_version.create
      ~dune_lang_version:(Dune_project.Dune_lang_version.create (3, 20))
  in
  test t {| (lang dune 4.5) |};
  [%expect {| (lang dune 3.20) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune_project.Dune_lang_version.Predicate.t as 'a
    constraint
      'a =
      [ `eq of Dune_project.Dune_lang_version.t
      | `gt of Dune_project.Dune_lang_version.t
      | `gte of Dune_project.Dune_lang_version.t
      | `lt of Dune_project.Dune_lang_version.t
      | `lte of Dune_project.Dune_lang_version.t
      | `neq of Dune_project.Dune_lang_version.t
      | `equals of Dune_project.Dune_lang_version.t
      | `greater_than_or_equal_to of Dune_project.Dune_lang_version.t
      | `less_than_or_equal_to of Dune_project.Dune_lang_version.t
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (lang dune 3.20) |} in
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`eq (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`eq (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`neq (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`neq (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_project.Dune_lang_version.create (3, 15))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_project.Dune_lang_version.create (3, 15))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_project.Dune_lang_version.create (3, 15))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_project.Dune_lang_version.create (4, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_project.Dune_lang_version.create (3, 20))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_project.Dune_lang_version.create (3, 15))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Dune_lang_version.enforce t ~condition);
      Dune_project_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (lang dune 3.20) |} in
  enforce t [];
  [%expect {| (lang dune 3.20) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ equals (Dune_project.Dune_lang_version.create (3, 20)) ];
  [%expect {| (lang dune 3.20) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ equals (Dune_project.Dune_lang_version.create (4, 5)) ];
  [%expect {| (lang dune 4.5) |}];
  let t = parse {| (lang dune 4.5) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (equals (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 4.5) |}];
  (* Blang. *)
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ true_ ];
  [%expect {| (lang dune 3.20) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce
    t
    [ and_
        [ not_ (equals (Dune_project.Dune_lang_version.create (4, 5)))
        ; equals (Dune_project.Dune_lang_version.create (3, 20))
        ]
    ];
  [%expect {| (lang dune 3.20) |}];
  enforce
    t
    [ or_
        [ equals (Dune_project.Dune_lang_version.create (3, 20))
        ; equals (Dune_project.Dune_lang_version.create (4, 5))
        ]
    ];
  [%expect {| (lang dune 3.20) |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (equals (Dune_project.Dune_lang_version.create (3, 20)))
      (equals (Dune_project.Dune_lang_version.create (4, 5)))
      (equals (Dune_project.Dune_lang_version.create (3, 20)))
  in
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ invariant ];
  [%expect {| (lang dune 4.5) |}];
  let t = parse {| (lang dune 2.8) |} in
  enforce t [ invariant ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ gte (Dune_project.Dune_lang_version.create (3, 18)) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ gte (Dune_project.Dune_lang_version.create (3, 18)) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ not_ (gte (Dune_project.Dune_lang_version.create (3, 18))) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce t [ lte (Dune_project.Dune_lang_version.create (3, 20)) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce t [ lte (Dune_project.Dune_lang_version.create (3, 20)) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce t [ not_ (lte (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 3.22) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ eq (Dune_project.Dune_lang_version.create (4, 5)) ];
  [%expect {| (lang dune 4.5) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ not_ (neq (Dune_project.Dune_lang_version.create (4, 5))) ];
  [%expect {| (lang dune 4.5) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ gte (Dune_project.Dune_lang_version.create (3, 18)) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ gte (Dune_project.Dune_lang_version.create (3, 18)) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ not_ (lt (Dune_project.Dune_lang_version.create (3, 18))) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce t [ lte (Dune_project.Dune_lang_version.create (3, 20)) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce t [ lte (Dune_project.Dune_lang_version.create (3, 20)) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce t [ not_ (gt (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ neq (Dune_project.Dune_lang_version.create (3, 20)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (!= 3.20))) |}];
  let t = parse {| (lang dune 3.20) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (eq (Dune_project.Dune_lang_version.create (3, 20))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (= 3.20)))) |}];
  let t = parse {| (lang dune 3.20) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ lt (Dune_project.Dune_lang_version.create (3, 18)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (< 3.18))) |}];
  let t = parse {| (lang dune 3.20) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (gte (Dune_project.Dune_lang_version.create (3, 18))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (>= 3.18)))) |}];
  let t = parse {| (lang dune 3.18) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ gt (Dune_project.Dune_lang_version.create (3, 20)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (> 3.20))) |}];
  let t = parse {| (lang dune 3.18) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (lte (Dune_project.Dune_lang_version.create (3, 20))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (<= 3.20)))) |}];
  (* Deprecated operators - testing for coverage. *)
  let t = parse {| (lang dune 3.17) |} in
  enforce
    t
    [ (greater_than_or_equal_to [@alert "-deprecated"])
        (Dune_project.Dune_lang_version.create (3, 18))
    ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce
    t
    [ (greater_than_or_equal_to [@alert "-deprecated"])
        (Dune_project.Dune_lang_version.create (3, 18))
    ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce
    t
    [ not_
        ((greater_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 18)))
    ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce
    t
    [ (less_than_or_equal_to [@alert "-deprecated"])
        (Dune_project.Dune_lang_version.create (3, 20))
    ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce
    t
    [ (less_than_or_equal_to [@alert "-deprecated"])
        (Dune_project.Dune_lang_version.create (3, 20))
    ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce
    t
    [ not_
        ((less_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 20)))
    ];
  [%expect {| (lang dune 3.22) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (lang dune 3.20) |} in
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (equals (Dune_project.Dune_lang_version.create (3, 20)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (gte (Dune_project.Dune_lang_version.create (3, 15)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (gte (Dune_project.Dune_lang_version.create (4, 5)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (lte (Dune_project.Dune_lang_version.create (4, 5)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (lte (Dune_project.Dune_lang_version.create (3, 15)))));
  [%expect {||}];
  (* Deprecated operators - testing for coverage. *)
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version
             ((greater_than_or_equal_to [@alert "-deprecated"])
                (Dune_project.Dune_lang_version.create (3, 15)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version
             ((greater_than_or_equal_to [@alert "-deprecated"])
                (Dune_project.Dune_lang_version.create (4, 5)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version
             ((less_than_or_equal_to [@alert "-deprecated"])
                (Dune_project.Dune_lang_version.create (4, 5)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version
             ((less_than_or_equal_to [@alert "-deprecated"])
                (Dune_project.Dune_lang_version.create (3, 15)))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:(`generate_opam_files is_present));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_project_linter.Dune_lang_version.Linter.eval t ~predicate:(`name true_));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_project_linter.Dune_lang_version.Linter.enforce t ~condition);
      Dune_project_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (lang dune 3.20) |} in
  enforce t [];
  [%expect {| (lang dune 3.20) |}];
  enforce t [ dune_lang_version (equals (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 3.20) |}];
  enforce t [ dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5))) ];
  [%expect {| (lang dune 4.5) |}];
  (* Enforcing other toplevel stanza has no effect. *)
  enforce t [ generate_opam_files is_present ];
  [%expect {| (lang dune 4.5) |}];
  enforce t [ name false_ ];
  [%expect {| (lang dune 4.5) |}];
  enforce t [ not_ (name false_) ];
  [%expect {| (lang dune 4.5) |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (lang dune 4.5) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce
    t
    [ dune_lang_version (not_ (equals (Dune_project.Dune_lang_version.create (3, 20)))) ];
  [%expect {| (lang dune 4.5) |}];
  enforce
    t
    [ and_
        [ dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5)))
        ; dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5)))
        ]
    ];
  [%expect {| (lang dune 4.5) |}];
  enforce
    t
    [ or_
        [ dune_lang_version (equals (Dune_project.Dune_lang_version.create (3, 20)))
        ; dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5)))
        ]
    ];
  [%expect {| (lang dune 4.5) |}];
  enforce
    t
    [ if_
        (dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5))))
        (dune_lang_version (equals (Dune_project.Dune_lang_version.create (3, 20))))
        (dune_lang_version (equals (Dune_project.Dune_lang_version.create (4, 5))))
    ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ dune_lang_version (gte (Dune_project.Dune_lang_version.create (3, 18))) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce t [ dune_lang_version (gte (Dune_project.Dune_lang_version.create (3, 18))) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce
    t
    [ dune_lang_version (not_ (gte (Dune_project.Dune_lang_version.create (3, 18)))) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce t [ dune_lang_version (lte (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce t [ dune_lang_version (lte (Dune_project.Dune_lang_version.create (3, 20))) ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce
    t
    [ dune_lang_version (not_ (lte (Dune_project.Dune_lang_version.create (3, 20)))) ];
  [%expect {| (lang dune 3.22) |}];
  (* Deprecated operators - testing for coverage. *)
  let t = parse {| (lang dune 3.17) |} in
  enforce
    t
    [ dune_lang_version
        ((greater_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 18)))
    ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.20) |} in
  enforce
    t
    [ dune_lang_version
        ((greater_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 18)))
    ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce
    t
    [ dune_lang_version
        (not_
           ((greater_than_or_equal_to [@alert "-deprecated"])
              (Dune_project.Dune_lang_version.create (3, 18))))
    ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce
    t
    [ dune_lang_version
        ((less_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 20)))
    ];
  [%expect {| (lang dune 3.20) |}];
  let t = parse {| (lang dune 3.18) |} in
  enforce
    t
    [ dune_lang_version
        ((less_than_or_equal_to [@alert "-deprecated"])
           (Dune_project.Dune_lang_version.create (3, 20)))
    ];
  [%expect {| (lang dune 3.18) |}];
  let t = parse {| (lang dune 3.22) |} in
  enforce
    t
    [ dune_lang_version
        (not_
           ((less_than_or_equal_to [@alert "-deprecated"])
              (Dune_project.Dune_lang_version.create (3, 20))))
    ];
  [%expect {| (lang dune 3.22) |}];
  ()
;;
