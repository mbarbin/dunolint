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
    (module Dunolint_linter.Dunolint_lang_version)
    ~path:(Fpath.v "dunolint")
    contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dunolint_linter.Dunolint_lang_version.write t))
  in
  test {| (lang dunolint 1.0) |};
  [%expect {| (lang dunolint 1.0) |}];
  test {| (lang dunolint 2.3) |};
  [%expect {| (lang dunolint 2.3) |}];
  test {| (lang dunolint 1.INVALID) |};
  [%expect
    {|
    File "dunolint", line 1, characters 16-25:
    Error: Invalid version format: "1.INVALID".
    [123]
    |}];
  test {| (lang dunolint invalid) |};
  [%expect
    {|
    File "dunolint", line 1, characters 16-23:
    Error: Expected VERSION.MINOR format, got: "invalid".
    [123]
    |}];
  test {| (lang invalid 1.0) |};
  [%expect
    {|
    File "dunolint", line 1, characters 7-14:
    Error: Expected (lang dunolint VERSION) format.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (lang dunolint 1.0) |} in
  print_s [%sexp (t : Dunolint_linter.Dunolint_lang_version.t)];
  [%expect {| ((dunolint_lang_version 1.0)) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dunolint_linter.Dunolint_lang_version.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (lang dunolint 1.0) |};
  [%expect {| (lang dunolint 1.0) |}];
  rewrite {| (lang dunolint 2.3) |};
  [%expect {| (lang dunolint 2.3) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (lang dunolint 1.0) |} ~f:(fun t ->
    print_s
      [%sexp
        (Dunolint_linter.Dunolint_lang_version.dunolint_lang_version t
         : Dunolint0.Dunolint_lang_version.t)];
    [%expect {| 1.0 |}];
    Dunolint_linter.Dunolint_lang_version.set_dunolint_lang_version
      t
      ~dunolint_lang_version:(Dunolint0.Dunolint_lang_version.create (2, 5));
    print_s
      [%sexp
        (Dunolint_linter.Dunolint_lang_version.dunolint_lang_version t
         : Dunolint0.Dunolint_lang_version.t)];
    [%expect {| 2.5 |}];
    ());
  [%expect {| (lang dunolint 2.5) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dunolint_linter.Dunolint_lang_version.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dunolint_linter.Dunolint_lang_version.create
      ~dunolint_lang_version:(Dunolint0.Dunolint_lang_version.create (1, 0))
  in
  test t {| (lang dunolint 2.3) |};
  [%expect {| (lang dunolint 1.0) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dunolint0.Dunolint_lang_version.Predicate.t as 'a
    constraint
      'a =
      [ `eq of Dunolint0.Dunolint_lang_version.t
      | `gt of Dunolint0.Dunolint_lang_version.t
      | `gte of Dunolint0.Dunolint_lang_version.t
      | `lt of Dunolint0.Dunolint_lang_version.t
      | `lte of Dunolint0.Dunolint_lang_version.t
      | `neq of Dunolint0.Dunolint_lang_version.t
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (lang dunolint 1.5) |} in
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`eq (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`eq (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`neq (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`neq (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gte (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gte (Dunolint0.Dunolint_lang_version.create (1, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gte (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gt (Dunolint0.Dunolint_lang_version.create (1, 0))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gt (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`gt (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lte (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lte (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lte (Dunolint0.Dunolint_lang_version.create (1, 0))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lt (Dunolint0.Dunolint_lang_version.create (2, 3))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lt (Dunolint0.Dunolint_lang_version.create (1, 5))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.eval
       t
       ~predicate:(`lt (Dunolint0.Dunolint_lang_version.create (1, 0))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dunolint_linter.Dunolint_lang_version.enforce t ~condition);
      Dunolint_linter.Dunolint_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [];
  [%expect {| (lang dunolint 1.5) |}];
  enforce t [ eq (Dunolint0.Dunolint_lang_version.create (1, 5)) ];
  [%expect {| (lang dunolint 1.5) |}];
  enforce t [ eq (Dunolint0.Dunolint_lang_version.create (2, 3)) ];
  [%expect {| (lang dunolint 2.3) |}];
  let t = parse {| (lang dunolint 2.3) |} in
  enforce t [ not_ (eq (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 2.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ true_ ];
  [%expect {| (lang dunolint 1.5) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ and_
        [ not_ (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))
        ; eq (Dunolint0.Dunolint_lang_version.create (1, 5))
        ]
    ];
  [%expect {| (lang dunolint 1.5) |}];
  enforce
    t
    [ or_
        [ eq (Dunolint0.Dunolint_lang_version.create (1, 5))
        ; eq (Dunolint0.Dunolint_lang_version.create (2, 3))
        ]
    ];
  [%expect {| (lang dunolint 1.5) |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))
      (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))
      (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))
  in
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ invariant ];
  [%expect {| (lang dunolint 2.3) |}];
  let t = parse {| (lang dunolint 1.0) |} in
  enforce t [ invariant ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce t [ gte (Dunolint0.Dunolint_lang_version.create (1, 3)) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ gte (Dunolint0.Dunolint_lang_version.create (1, 3)) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce t [ not_ (gte (Dunolint0.Dunolint_lang_version.create (1, 3))) ];
  [%expect {| (lang dunolint 1.2) |}];
  let t = parse {| (lang dunolint 1.7) |} in
  enforce t [ lte (Dunolint0.Dunolint_lang_version.create (1, 5)) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  enforce t [ lte (Dunolint0.Dunolint_lang_version.create (1, 5)) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.7) |} in
  enforce t [ not_ (lte (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 1.7) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ eq (Dunolint0.Dunolint_lang_version.create (2, 3)) ];
  [%expect {| (lang dunolint 2.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ not_ (neq (Dunolint0.Dunolint_lang_version.create (2, 3))) ];
  [%expect {| (lang dunolint 2.3) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce t [ gte (Dunolint0.Dunolint_lang_version.create (1, 3)) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [ gte (Dunolint0.Dunolint_lang_version.create (1, 3)) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce t [ not_ (lt (Dunolint0.Dunolint_lang_version.create (1, 3))) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.7) |} in
  enforce t [ lte (Dunolint0.Dunolint_lang_version.create (1, 5)) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  enforce t [ lte (Dunolint0.Dunolint_lang_version.create (1, 5)) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  enforce t [ not_ (gt (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  require_does_raise (fun () ->
    enforce t [ neq (Dunolint0.Dunolint_lang_version.create (1, 5)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (!= 1.5))) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (eq (Dunolint0.Dunolint_lang_version.create (1, 5))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (= 1.5)))) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  require_does_raise (fun () ->
    enforce t [ lt (Dunolint0.Dunolint_lang_version.create (1, 3)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (< 1.3))) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (gte (Dunolint0.Dunolint_lang_version.create (1, 3))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (>= 1.3)))) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  require_does_raise (fun () ->
    enforce t [ gt (Dunolint0.Dunolint_lang_version.create (1, 5)) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (> 1.5))) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  require_does_raise (fun () ->
    enforce t [ not_ (lte (Dunolint0.Dunolint_lang_version.create (1, 5))) ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (<= 1.5)))) |}];
  ()
;;

let%expect_test "Linter.eval" =
  let _, t = parse {| (lang dunolint 1.5) |} in
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (gte (Dunolint0.Dunolint_lang_version.create (1, 0)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (gte (Dunolint0.Dunolint_lang_version.create (2, 3)))));
  [%expect {||}];
  Test_helpers.is_true
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (lte (Dunolint0.Dunolint_lang_version.create (2, 3)))));
  [%expect {||}];
  Test_helpers.is_false
    (Dunolint_linter.Dunolint_lang_version.Linter.eval
       t
       ~predicate:
         (`dunolint_lang_version (lte (Dunolint0.Dunolint_lang_version.create (1, 0)))));
  [%expect {||}];
  ()
;;

let%expect_test "Linter.enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dunolint_linter.Dunolint_lang_version.Linter.enforce t ~condition);
      Dunolint_linter.Dunolint_lang_version.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (lang dunolint 1.5) |} in
  enforce t [];
  [%expect {| (lang dunolint 1.5) |}];
  enforce t [ dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 1.5) |}];
  enforce t [ dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3))) ];
  [%expect {| (lang dunolint 2.3) |}];
  (* Enforcing negation of the predicate: condition is evaluated, no auto-fix. *)
  enforce
    t
    [ not_ (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))) ];
  [%expect {| (lang dunolint 2.3) |}];
  require_does_raise (fun () ->
    enforce
      t
      [ not_ (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3))))
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (dunolint_lang_version (= 2.3)))))
    |}];
  (* Blang. *)
  enforce t [ true_ ];
  [%expect {| (lang dunolint 2.3) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ dunolint_lang_version (not_ (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))) ];
  [%expect {| (lang dunolint 2.3) |}];
  enforce
    t
    [ and_
        [ dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))
        ; dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))
        ]
    ];
  [%expect {| (lang dunolint 2.3) |}];
  enforce
    t
    [ or_
        [ dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5)))
        ; dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3)))
        ]
    ];
  [%expect {| (lang dunolint 2.3) |}];
  enforce
    t
    [ if_
        (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3))))
        (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (1, 5))))
        (dunolint_lang_version (eq (Dunolint0.Dunolint_lang_version.create (2, 3))))
    ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce
    t
    [ dunolint_lang_version (gte (Dunolint0.Dunolint_lang_version.create (1, 3))) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.5) |} in
  enforce
    t
    [ dunolint_lang_version (gte (Dunolint0.Dunolint_lang_version.create (1, 3))) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.2) |} in
  enforce
    t
    [ dunolint_lang_version (not_ (gte (Dunolint0.Dunolint_lang_version.create (1, 3)))) ];
  [%expect {| (lang dunolint 1.2) |}];
  let t = parse {| (lang dunolint 1.7) |} in
  enforce
    t
    [ dunolint_lang_version (lte (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 1.5) |}];
  let t = parse {| (lang dunolint 1.3) |} in
  enforce
    t
    [ dunolint_lang_version (lte (Dunolint0.Dunolint_lang_version.create (1, 5))) ];
  [%expect {| (lang dunolint 1.3) |}];
  let t = parse {| (lang dunolint 1.7) |} in
  enforce
    t
    [ dunolint_lang_version (not_ (lte (Dunolint0.Dunolint_lang_version.create (1, 5)))) ];
  [%expect {| (lang dunolint 1.7) |}];
  ()
;;
