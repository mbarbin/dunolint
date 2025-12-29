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
  Test_helpers.parse (module Dune_linter.Preprocess) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Preprocess.write t))
  in
  test {| (preprocess no_preprocessing) |};
  [%expect {| (preprocess no_preprocessing) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [preprocess] field.
    [123]
    |}];
  test {| (preprocess (pps ppx_sexp_conv)) |};
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  test {| (preprocess (pps ppx_sexp_conv -unused-code-warnings=force)) |};
  [%expect {| (preprocess (pps ppx_sexp_conv -unused-code-warnings=force)) |}];
  test {| (preprocess other) |};
  [%expect {| (preprocess other) |}];
  test {| (preprocess (something else)) |};
  [%expect {| (preprocess (something else)) |}];
  test {| (preprocess something else) |};
  [%expect
    {|
    File "dune", line 1, characters 1-28:
    Error: Unexpected [preprocess] field value.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (preprocess no_preprocessing) |} in
  print_s [%sexp (t : Dune_linter.Preprocess.t)];
  [%expect {| ((state No_preprocessing)) |}];
  let _, t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  print_s [%sexp (t : Dune_linter.Preprocess.t)];
  [%expect
    {|
    ((state
      (Pps
       ((sections
         (((entries (((arg (Pp (pp_name ppx_sexp_conv))) (eol_comment ())))))))))))
    |}];
  let _, t = parse {| (preprocess (something else)) |} in
  print_s [%sexp (t : Dune_linter.Preprocess.t)];
  [%expect {| ((state (Unhandled (something else)))) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Preprocess.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (preprocess no_preprocessing) |};
  [%expect {| (preprocess no_preprocessing) |}];
  rewrite {| (preprocess (pps ppx_sexp_conv)) |};
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  rewrite {| (preprocess (something else)) |};
  [%expect {| (preprocess (something else)) |}];
  require_does_raise (fun () -> rewrite {| (preprocess something else) |});
  [%expect {| "Unexpected [preprocess] field value." |}];
  rewrite {| (preprocess (pps ppx_sexp_conv)) |} ~f:(fun t ->
    let open Dunolint.Config.Std in
    Dune_linter.Preprocess.enforce t ~condition:no_preprocessing);
  [%expect {| (preprocess no_preprocessing) |}];
  (* Exercising some getters and setters. *)
  rewrite {| (preprocess no_preprocessing) |} ~f:(fun t ->
    print_s [%sexp (Dune_linter.Preprocess.state t : Dune_linter.Preprocess.State.t)];
    [%expect {| No_preprocessing |}];
    Dune_linter.Preprocess.set_state t ~state:(Unhandled (Atom "foo")));
  [%expect {| (preprocess no_preprocessing) |}];
  rewrite {| (preprocess (pps ppx_sexp_conv)) |} ~f:(fun t ->
    Dune_linter.Preprocess.set_state t ~state:(Unhandled (Atom "foo")));
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  ()
;;

let%expect_test "unhandled_rewrite" =
  let sexps_rewriter, field =
    Test_helpers.read_sexp_field ~path:(Fpath.v "a/dune") "(preprocess something else)"
  in
  let t = Dune_linter.Preprocess.create () in
  require_does_raise (fun () -> Dune_linter.Preprocess.rewrite t ~sexps_rewriter ~field);
  [%expect {| "Unexpected [preprocess] field value." |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Preprocess.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Preprocess.create () in
  test t {| (preprocess no_preprocessing) |};
  [%expect {| (preprocess no_preprocessing) |}];
  test t {| (preprocess (pps ppx_sexp_conv)) |};
  [%expect {| (preprocess no_preprocessing) |}];
  test t {| (preprocess (something else)) |};
  [%expect {| (preprocess no_preprocessing) |}];
  let t =
    Dune_linter.Preprocess.create
      ~pps:
        (Dune_linter.Pps.create
           ~args:
             [ Pp (Dune.Pp.Name.v "ppx_compare")
             ; Flag { name = "-foo"; param = None }
             ; Flag { name = "--bar"; param = Some "1" }
             ])
      ()
  in
  test t {| (preprocess no_preprocessing) |};
  [%expect {| (preprocess (pps ppx_compare --bar=1 -foo)) |}];
  test t {| (preprocess (pps ppx_sexp_conv)) |};
  [%expect {| (preprocess (pps ppx_compare --bar=1 -foo)) |}];
  test t {| (preprocess (something else)) |};
  [%expect {| (preprocess (pps ppx_compare --bar=1 -foo)) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Preprocess.Predicate.t as 'a
    constraint 'a = [ `no_preprocessing | `pps of Dune.Pps.Predicate.t Blang.t ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  Test_helpers.is_true
    (Dune_linter.Preprocess.eval
       t
       ~predicate:(`pps (pp (Dune.Pp.Name.v "ppx_sexp_conv"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Preprocess.eval t ~predicate:(`pps (pp (Dune.Pp.Name.v "ppx_other"))));
  [%expect {||}];
  Test_helpers.is_false (Dune_linter.Preprocess.eval t ~predicate:`no_preprocessing);
  [%expect {||}];
  let _, t = parse {| (preprocess no_preprocessing) |} in
  Test_helpers.is_true (Dune_linter.Preprocess.eval t ~predicate:`no_preprocessing);
  [%expect {||}];
  let _, t = parse {| (preprocess (something else)) |} in
  Test_helpers.is_false (Dune_linter.Preprocess.eval t ~predicate:`no_preprocessing);
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Preprocess.eval t ~predicate:(`pps (pp (Dune.Pp.Name.v "ppx_other"))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Preprocess.enforce t ~condition);
      Dune_linter.Preprocess.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  enforce t [];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  (* Enforcing the presence of a present pp has no effect. *)
  enforce t [ pps (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  (* Enforcing its negation triggers an error. *)
  require_does_raise (fun () ->
    enforce t [ not_ (pps (pp (Dune.Pp.Name.v "ppx_sexp_conv"))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not (pps (pp ppx_sexp_conv)))))
    |}];
  (* Enforcing the presence of a new pp adds it. *)
  enforce t [ pps (pp (Dune.Pp.Name.v "ppx_other")) ];
  [%expect {| (preprocess (pps ppx_other ppx_sexp_conv)) |}];
  (* Enforcing the absence of an absent pp has no effect. *)
  enforce t [ pps (not_ (pp (Dune.Pp.Name.v "ppx_not_there"))) ];
  [%expect {| (preprocess (pps ppx_other ppx_sexp_conv)) |}];
  (* Enforcing the negation of a present pp removes it. *)
  enforce t [ pps (not_ (pp (Dune.Pp.Name.v "ppx_sexp_conv"))) ];
  [%expect {| (preprocess (pps ppx_other)) |}];
  (* no_preprocessing *)
  let t = parse {| (preprocess no_preprocessing) |} in
  (* Enforcing [no_preprocessing] does nothing if it is already set. *)
  enforce t [ no_preprocessing ];
  [%expect {| (preprocess no_preprocessing) |}];
  (* Enforcing the negation of [no_preprocessing] triggers an error. *)
  require_does_raise (fun () -> enforce t [ not_ no_preprocessing ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (not no_preprocessing)))
    |}];
  (* Enforcing the presence of a present pp adds a pps section with it. *)
  let t = parse {| (preprocess no_preprocessing) |} in
  enforce t [ pps (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  (* This is also the case if the state is current unknown. *)
  let t = parse {| (preprocess (something else)) |} in
  enforce t [ pps (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  (* Blang. *)
  let t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  enforce t [ true_ ];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  require_does_raise (fun () -> enforce t [ false_ ]);
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition false)) |}];
  enforce
    t
    [ pps
        (and_
           [ pp (Dune.Pp.Name.v "ppx_other"); not_ (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ])
    ];
  [%expect {| (preprocess (pps ppx_other)) |}];
  (* [or] does not have an enforcement strategy when its invariant is not
     satisfied. *)
  enforce t [ or_ [ pps (pp (Dune.Pp.Name.v "ppx_other")); no_preprocessing ] ];
  [%expect {| (preprocess (pps ppx_other)) |}];
  require_does_raise (fun () ->
    enforce t [ or_ [ pps (pp (Dune.Pp.Name.v "ppx_absent")); no_preprocessing ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (or (pps (pp ppx_absent)) no_preprocessing)))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (pps (pp (Dune.Pp.Name.v "ppx_sexp_conv")))
      (pps (pp (Dune.Pp.Name.v "ppx_other")))
      no_preprocessing
  in
  let t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  enforce t [ invariant ];
  [%expect {| (preprocess (pps ppx_other ppx_sexp_conv)) |}];
  let t = parse {| (preprocess no_preprocessing) |} in
  enforce t [ invariant ];
  [%expect {| (preprocess no_preprocessing) |}];
  ()
;;
