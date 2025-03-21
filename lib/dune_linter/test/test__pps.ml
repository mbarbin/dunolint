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
      let t = Dune_linter.Pps.read ~sexps_rewriter ~field in
      print_s (Dune_linter.Pps.write t))
  in
  test {| (pps ppx_deriving) |};
  [%expect {| (pps ppx_deriving) |}];
  test {| (pps (ppx_deriving -deriving)) |};
  [%expect
    {|
    File "dune", line 1, characters 6-30:
    Error: Unexpected [Sexp.List]. [Pps] expected to be atoms.
    [123]
    |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [pps] field.
    [123]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let sexps_rewriter, field = Common.read {| (pps ppx_deriving) |} in
  let t = Dune_linter.Pps.read ~sexps_rewriter ~field in
  print_s [%sexp (t : Dune_linter.Pps.t)];
  [%expect {| ((args ((Pp (pp_name ppx_deriving))))) |}];
  ()
;;

let parse str =
  let sexps_rewriter, field = Common.read str in
  let t = Dune_linter.Pps.read ~sexps_rewriter ~field in
  sexps_rewriter, field, t
;;

let rewrite ?(f = ignore) str =
  let sexps_rewriter, field, t = parse str in
  f t;
  Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (pps ppx_deriving) |};
  [%expect {| (pps ppx_deriving) |}];
  (* Exercising some getters. *)
  rewrite {| (pps ppx_deriving -flag) |} ~f:(fun t ->
    (* There are no getters to test at the moment. *)
    ignore (t : Dune_linter.Pps.t);
    ());
  [%expect {| (pps ppx_deriving -flag) |}];
  (* Exercising some setters. *)
  rewrite {| (pps ppx_deriving -flag) |} ~f:(fun t ->
    (* There are no setters to test at the moment. *)
    ignore (t : Dune_linter.Pps.t);
    ());
  [%expect {| (pps ppx_deriving -flag) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Pps.create ~args:[ Pp (Dune.Pp.Name.v "ppx_deriving") ] in
  test t {| (pps -flag ppx_deriving -flag) |};
  [%expect {| (pps ppx_deriving) |}];
  (* When dunolint doesn't understand the expression to rewrite, this triggers an error. *)
  require_does_raise [%here] (fun () -> test t {| (other_field (unexpected args)) |});
  [%expect {| ("Unexpected [pps] field." (Exit 123)) |}];
  (* However if the field is [pps] the existing arguments are replaced. *)
  test t {| (pps (unexpected args)) |};
  [%expect {| (pps ppx_deriving) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Pps.Predicate.t as 'a
    constraint
      'a =
      [ `pp of Dune.Pp.Name.t
      | `flag of Dune.Pps.Predicate.Flag.t
      | `pp_with_flag of Dune.Pps.Predicate.Pp_with_flag.t
      ]
end

open Dunolint.Config.Std

let is_true b = require_equal [%here] (module Dunolint.Trilang) b True
let is_false b = require_equal [%here] (module Dunolint.Trilang) b False

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let parse str =
    let _, _, t = parse str in
    t
  in
  let t = parse {| (pps ppx_deriving) |} in
  is_true (Dune_linter.Pps.eval t ~predicate:(`pp (Dune.Pp.Name.v "ppx_deriving")));
  [%expect {| |}];
  is_false (Dune_linter.Pps.eval t ~predicate:(`pp (Dune.Pp.Name.v "ppx_other")));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce (sexps_rewriter, field, t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Pps.enforce t ~condition);
      Dune_linter.Pps.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (pps ppx_sexp_conv) |} in
  enforce t [];
  [%expect {| (pps ppx_sexp_conv) |}];
  (* Enforcing the presence of a present pp has no effect. *)
  enforce t [ pp (Dune.Pp.Name.v "ppx_sexp_conv") ];
  [%expect {| (pps ppx_sexp_conv) |}];
  (* Enforcing the presence of a new pp adds it. *)
  enforce t [ pp (Dune.Pp.Name.v "ppx_other") ];
  [%expect {| (pps ppx_other ppx_sexp_conv) |}];
  (* Enforcing the absence of an absent pp has no effect. *)
  enforce t [ not_ (pp (Dune.Pp.Name.v "ppx_not_there")) ];
  [%expect {| (pps ppx_other ppx_sexp_conv) |}];
  (* Enforcing the negation of a present pp removes it. *)
  enforce t [ not_ (pp (Dune.Pp.Name.v "ppx_sexp_conv")) ];
  [%expect {| (pps ppx_other) |}];
  (* Blang. *)
  let t = parse {| (pps ppx_deriving) |} in
  enforce t [ true_ ];
  [%expect {| (pps ppx_deriving) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
  enforce
    t
    [ and_ [ not_ (pp (Dune.Pp.Name.v "ppx_other")); pp (Dune.Pp.Name.v "ppx_deriving") ]
    ];
  [%expect {| (pps ppx_deriving) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce
    t
    [ or_ [ pp (Dune.Pp.Name.v "ppx_deriving"); pp (Dune.Pp.Name.v "ppx_other") ] ];
  [%expect {| (pps ppx_deriving) |}];
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ pp (Dune.Pp.Name.v "qualified"); pp (Dune.Pp.Name.v "no") ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (pp qualified)
        (pp no))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (pp (Dune.Pp.Name.v "ppx_deriving"))
      (pp (Dune.Pp.Name.v "ppx_other"))
      (pp (Dune.Pp.Name.v "ppx_deriving"))
  in
  let t = parse {| (pps ppx_deriving) |} in
  enforce t [ invariant ];
  [%expect {| (pps ppx_deriving ppx_other) |}];
  let t = parse {| (pps ppx_other) |} in
  enforce t [ invariant ];
  [%expect {| (pps ppx_deriving ppx_other) |}];
  ()
;;
