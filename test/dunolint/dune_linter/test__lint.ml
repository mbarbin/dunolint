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
  Test_helpers.parse (module Dune_linter.Lint) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Lint.write t))
  in
  test {| (lint (pps ppx_js_style)) |};
  [%expect {| (lint (pps ppx_js_style)) |}];
  test {| (lint (pps ppx_js_style -check-doc-comments)) |};
  [%expect {| (lint (pps ppx_js_style -check-doc-comments)) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [lint] field.
    [123]
    |}];
  test {| (lint (invalid field)) |};
  [%expect
    {|
    File "dune", line 1, characters 1-23:
    Error: Unexpected [lint] field value.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (lint (pps ppx_js_style)) |} in
  print_s [%sexp (t : Dune_linter.Lint.t)];
  [%expect
    {|
    ((pps
      ((sections
        (((entries (((arg (Pp (pp_name ppx_js_style))) (eol_comment ()))))))))))
    |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Lint.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (lint (pps ppx_js_style)) |};
  [%expect {| (lint (pps ppx_js_style)) |}];
  (* Exercising some getters. *)
  rewrite {| (lint (pps ppx_js_style -check-doc-comments)) |} ~f:(fun t ->
    (* There are no getters to test at the moment. *)
    ignore (t : Dune_linter.Lint.t);
    ());
  [%expect {| (lint (pps ppx_js_style -check-doc-comments)) |}];
  (* Exercising some setters. *)
  rewrite {| (lint (pps ppx_js_style -check-doc-comments)) |} ~f:(fun t ->
    (* There are no setters to test at the moment. *)
    ignore (t : Dune_linter.Lint.t);
    ());
  [%expect {| (lint (pps ppx_js_style -check-doc-comments)) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Lint.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Lint.create
      ~pps:(Dune_linter.Pps.create ~args:[ Pp (Dune.Pp.Name.v "ppx_js_style") ])
      ()
  in
  test t {| (lint (pps ppx_js_style -check-doc-comments)) |};
  [%expect {| (lint (pps ppx_js_style)) |}];
  let t = Dune_linter.Lint.create () in
  test t {| (lint (pps ppx_js_style -check-doc-comments)) |};
  [%expect {| (lint (pps)) |}];
  (* When dunolint doesn't understand the expression to rewrite, this triggers an error. *)
  require_does_raise (fun () -> test t {| (lint (unexpected args)) |});
  [%expect {| "Unexpected [lint] field value." |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Lint.Predicate.t as 'a
    constraint 'a = [ `pps of Dune.Pps.Predicate.t Blang.t ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (lint (pps ppx_js_style)) |} in
  Test_helpers.is_true
    (Dune_linter.Lint.eval
       t
       ~predicate:(`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style")))));
  [%expect {| |}];
  Test_helpers.is_false
    (Dune_linter.Lint.eval
       t
       ~predicate:(`pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other")))));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Lint.enforce t ~condition);
      Dune_linter.Lint.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (lint (pps ppx_js_style)) |} in
  enforce t [];
  [%expect {| (lint (pps ppx_js_style)) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style"))) ];
  [%expect {| (lint (pps ppx_js_style)) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other"))) ];
  [%expect {| (lint (pps ppx_js_style ppx_other)) |}];
  let t = parse {| (lint (pps ppx_other)) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style")))) ];
  [%expect {| (lint (pps ppx_other)) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new setting, this
     requires the user's intervention. *)
  require_does_raise (fun () ->
    enforce t [ not_ (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other")))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (pps (pp ppx_other)))))
    |}];
  (* Blang. *)
  let t = parse {| (lint (pps ppx_js_style)) |} in
  enforce t [ true_ ];
  [%expect {| (lint (pps ppx_js_style)) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ and_
        [ not_ (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other"))))
        ; pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style")))
        ]
    ];
  [%expect {| (lint (pps ppx_js_style)) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce
    t
    [ or_
        [ pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style")))
        ; pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other")))
        ]
    ];
  [%expect {| (lint (pps ppx_js_style)) |}];
  require_does_raise (fun () ->
    enforce
      t
      [ or_
          [ pps (Blang.base (`pp (Dune.Pp.Name.v "qualified")))
          ; pps (Blang.base (`pp (Dune.Pp.Name.v "no")))
          ]
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (or (pps (pp qualified)) (pps (pp no)))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style"))))
      (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_other"))))
      (pps (Blang.base (`pp (Dune.Pp.Name.v "ppx_js_style"))))
  in
  let t = parse {| (lint (pps ppx_js_style)) |} in
  enforce t [ invariant ];
  [%expect {| (lint (pps ppx_js_style ppx_other)) |}];
  let t = parse {| (lint (pps ppx_other)) |} in
  enforce t [ invariant ];
  [%expect {| (lint (pps ppx_js_style ppx_other)) |}];
  ()
;;
