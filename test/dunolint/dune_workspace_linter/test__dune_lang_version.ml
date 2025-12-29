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
    (module Dune_workspace_linter.Dune_lang_version)
    ~path:(Fpath.v "dune-workspace")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_workspace_linter.Dune_lang_version.write t))
  in
  test {| (lang dune 3.17) |};
  [%expect {| (lang dune 3.17) |}];
  test {| (lang dune 4.0) |};
  [%expect {| (lang dune 4.0) |}];
  test {| (lang dune 3.INVALID) |};
  [%expect
    {|
    File "dune-workspace", line 1, characters 12-21:
    Error: Invalid version format: "3.INVALID".
    [123]
    |}];
  test {| (lang dune invalid) |};
  [%expect
    {|
    File "dune-workspace", line 1, characters 12-19:
    Error: Expected VERSION.MINOR format, got: "invalid".
    [123]
    |}];
  test {| (lang invalid 3.17) |};
  [%expect
    {|
    File "dune-workspace", line 1, characters 7-14:
    Error: Expected (lang dune VERSION) format.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (lang dune 3.17) |} in
  print_s [%sexp (t : Dune_workspace_linter.Dune_lang_version.t)];
  [%expect {| ((dune_lang_version 3.17)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_workspace_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (lang dune 3.17) |};
  [%expect {| (lang dune 3.17) |}];
  rewrite {| (lang dune 4.0) |};
  [%expect {| (lang dune 4.0) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (lang dune 3.17) |} ~f:(fun t ->
    print_s
      [%sexp
        (Dune_workspace_linter.Dune_lang_version.dune_lang_version t
         : Dune_workspace.Dune_lang_version.t)];
    [%expect {| 3.17 |}];
    Dune_workspace_linter.Dune_lang_version.set_dune_lang_version
      t
      ~dune_lang_version:(Dune_workspace.Dune_lang_version.create (4, 5));
    print_s
      [%sexp
        (Dune_workspace_linter.Dune_lang_version.dune_lang_version t
         : Dune_workspace.Dune_lang_version.t)];
    [%expect {| 4.5 |}];
    ());
  [%expect {| (lang dune 4.5) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_workspace_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_workspace_linter.Dune_lang_version.create
      ~dune_lang_version:(Dune_workspace.Dune_lang_version.create (3, 17))
  in
  test t {| (lang dune 4.0) |};
  [%expect {| (lang dune 3.17) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune_workspace.Dune_lang_version.Predicate.t as 'a
    constraint
      'a =
      [ `eq of Dune_workspace.Dune_lang_version.t
      | `gt of Dune_workspace.Dune_lang_version.t
      | `gte of Dune_workspace.Dune_lang_version.t
      | `lt of Dune_workspace.Dune_lang_version.t
      | `lte of Dune_workspace.Dune_lang_version.t
      | `neq of Dune_workspace.Dune_lang_version.t
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (lang dune 3.17) |} in
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`eq (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`eq (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`neq (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`neq (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_workspace.Dune_lang_version.create (3, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gte (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_workspace.Dune_lang_version.create (3, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`gt (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lte (Dune_workspace.Dune_lang_version.create (3, 0))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_workspace.Dune_lang_version.create (4, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_workspace.Dune_lang_version.create (3, 17))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.eval
       t
       ~predicate:(`lt (Dune_workspace.Dune_lang_version.create (3, 0))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_workspace_linter.Dune_lang_version.enforce t ~condition);
      Dune_workspace_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (lang dune 3.17) |} in
  enforce t [];
  [%expect {| (lang dune 3.17) |}];
  enforce t [ eq (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.17) |}];
  enforce t [ eq (Dune_workspace.Dune_lang_version.create (4, 0)) ];
  [%expect {| (lang dune 4.0) |}];
  let t = parse {| (lang dune 4.0) |} in
  enforce t [ not_ (eq (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 4.0) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ true_ ];
  [%expect {| (lang dune 3.17) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ and_
        [ not_ (eq (Dune_workspace.Dune_lang_version.create (4, 0)))
        ; eq (Dune_workspace.Dune_lang_version.create (3, 17))
        ]
    ];
  [%expect {| (lang dune 3.17) |}];
  enforce
    t
    [ or_
        [ eq (Dune_workspace.Dune_lang_version.create (3, 17))
        ; eq (Dune_workspace.Dune_lang_version.create (4, 0))
        ]
    ];
  [%expect {| (lang dune 3.17) |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (eq (Dune_workspace.Dune_lang_version.create (3, 17)))
      (eq (Dune_workspace.Dune_lang_version.create (4, 0)))
      (eq (Dune_workspace.Dune_lang_version.create (3, 17)))
  in
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ invariant ];
  [%expect {| (lang dune 4.0) |}];
  let t = parse {| (lang dune 3.0) |} in
  enforce t [ invariant ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ gte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ gte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.19) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ not_ (gte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ lte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ lte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ not_ (lte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.19) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ eq (Dune_workspace.Dune_lang_version.create (4, 0)) ];
  [%expect {| (lang dune 4.0) |}];
  let t = parse {| (lang dune 3.17) |} in
  enforce t [ not_ (neq (Dune_workspace.Dune_lang_version.create (4, 0))) ];
  [%expect {| (lang dune 4.0) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ gte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ gte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.19) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ not_ (lt (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ lte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ lte (Dune_workspace.Dune_lang_version.create (3, 17)) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ not_ (gt (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.17) |} in
  require_does_raise (fun () ->
    enforce t [ neq (Dune_workspace.Dune_lang_version.create (3, 17)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (!= 3.17))) |}];
  let t = parse {| (lang dune 3.17) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (eq (Dune_workspace.Dune_lang_version.create (3, 17))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (= 3.17)))) |}];
  let t = parse {| (lang dune 3.17) |} in
  require_does_raise (fun () ->
    enforce t [ lt (Dune_workspace.Dune_lang_version.create (3, 10)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (< 3.10))) |}];
  let t = parse {| (lang dune 3.17) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (gte (Dune_workspace.Dune_lang_version.create (3, 10))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (>= 3.10)))) |}];
  let t = parse {| (lang dune 3.10) |} in
  require_does_raise (fun () ->
    enforce t [ gt (Dune_workspace.Dune_lang_version.create (3, 17)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (> 3.17))) |}];
  let t = parse {| (lang dune 3.10) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (lte (Dune_workspace.Dune_lang_version.create (3, 17))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (<= 3.17)))) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (lang dune 3.17) |} in
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (3, 17)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (gte (Dune_workspace.Dune_lang_version.create (3, 0)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (gte (Dune_workspace.Dune_lang_version.create (4, 0)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (lte (Dune_workspace.Dune_lang_version.create (4, 0)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_workspace_linter.Dune_lang_version.Linter.eval
       t
       ~predicate:
         (`dune_lang_version (lte (Dune_workspace.Dune_lang_version.create (3, 0)))));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_workspace_linter.Dune_lang_version.Linter.enforce t ~condition);
      Dune_workspace_linter.Dune_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (lang dune 3.17) |} in
  enforce t [];
  [%expect {| (lang dune 3.17) |}];
  enforce t [ dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.17) |}];
  enforce t [ dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0))) ];
  [%expect {| (lang dune 4.0) |}];
  (* Enforcing negation of the predicate: condition is evaluated, no auto-fix. *)
  enforce
    t
    [ not_ (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (3, 17)))) ];
  [%expect {| (lang dune 4.0) |}];
  require_does_raise (fun () ->
    enforce
      t
      [ not_ (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0)))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (dune_lang_version (= 4.0)))))
    |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (lang dune 4.0) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ dune_lang_version (not_ (eq (Dune_workspace.Dune_lang_version.create (3, 17)))) ];
  [%expect {| (lang dune 4.0) |}];
  enforce
    t
    [ and_
        [ dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0)))
        ; dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0)))
        ]
    ];
  [%expect {| (lang dune 4.0) |}];
  enforce
    t
    [ or_
        [ dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (3, 17)))
        ; dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0)))
        ]
    ];
  [%expect {| (lang dune 4.0) |}];
  enforce
    t
    [ if_
        (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0))))
        (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (3, 17))))
        (dune_lang_version (eq (Dune_workspace.Dune_lang_version.create (4, 0))))
    ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ dune_lang_version (gte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ dune_lang_version (gte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.19) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce
    t
    [ dune_lang_version (not_ (gte (Dune_workspace.Dune_lang_version.create (3, 17)))) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce t [ dune_lang_version (lte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.17) |}];
  let t = parse {| (lang dune 3.10) |} in
  enforce t [ dune_lang_version (lte (Dune_workspace.Dune_lang_version.create (3, 17))) ];
  [%expect {| (lang dune 3.10) |}];
  let t = parse {| (lang dune 3.19) |} in
  enforce
    t
    [ dune_lang_version (not_ (lte (Dune_workspace.Dune_lang_version.create (3, 17)))) ];
  [%expect {| (lang dune 3.19) |}];
  ()
;;
