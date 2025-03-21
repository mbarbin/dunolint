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

open Dunolint.Config.Std

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let sexps_rewriter, field = Common.read contents in
      let t =
        try Dune_linter.Preprocess.read ~sexps_rewriter ~field with
        | Sexp.Of_sexp_error (_, sexp) ->
          (* We redact the message because it contains paths to source files,
             which is inconvenient when relocating the code in sub repos. *)
          raise_s
            [%sexp Of_sexp_error, ("_", { invalid_sexp = (sexp : Sexp.t) })]
          [@coverage off]
      in
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
  ()
;;

let%expect_test "sexp_of" =
  let sexps_rewriter, field = Common.read {| (preprocess (pps ppx_sexp_conv)) |} in
  let t = Dune_linter.Preprocess.read ~sexps_rewriter ~field in
  print_s [%sexp (t : Dune_linter.Preprocess.t)];
  [%expect {| ((state (Pps ((args ((Pp (pp_name ppx_sexp_conv)))))))) |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Preprocess.Predicate.t as 'a
    constraint 'a = [ `no_preprocessing | `pps of Dune.Pps.Predicate.t Blang.t ]
end

let parse str =
  let sexps_rewriter, field = Common.read str in
  let t = Dune_linter.Preprocess.read ~sexps_rewriter ~field in
  sexps_rewriter, field, t
;;

open Dunolint.Config.Std

let is_true b = require_equal [%here] (module Dunolint.Trilang) b True
let is_false b = require_equal [%here] (module Dunolint.Trilang) b False

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let parse str =
    let _, _, t = parse str in
    t
  in
  let t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  is_true
    (Dune_linter.Preprocess.eval
       t
       ~predicate:(`pps (pp (Dune.Pp.Name.v "ppx_sexp_conv"))));
  [%expect {| |}];
  is_false
    (Dune_linter.Preprocess.eval t ~predicate:(`pps (pp (Dune.Pp.Name.v "ppx_other"))));
  [%expect {| |}];
  is_false (Dune_linter.Preprocess.eval t ~predicate:`no_preprocessing);
  [%expect {| |}];
  let sexps_rewriter, field = Common.read {| (preprocess no_preprocessing) |} in
  let t = Dune_linter.Preprocess.read ~sexps_rewriter ~field in
  is_true (Dune_linter.Preprocess.eval t ~predicate:`no_preprocessing);
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce (sexps_rewriter, field, t) conditions =
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
  (* Enforcing the presence of a new pp adds it. *)
  enforce t [ pps (pp (Dune.Pp.Name.v "ppx_other")) ];
  [%expect {| (preprocess (pps ppx_other ppx_sexp_conv)) |}];
  (* Enforcing the non-equality with another value has no effect. *)
  enforce t [ pps (not_ (pp (Dune.Pp.Name.v "ppx_not_there"))) ];
  [%expect {| (preprocess (pps ppx_other ppx_sexp_conv)) |}];
  (* Enforcing the negation of a present removes it. *)
  enforce t [ pps (not_ (pp (Dune.Pp.Name.v "ppx_sexp_conv"))) ];
  [%expect {| (preprocess (pps ppx_other)) |}];
  (* no_preprocessing *)
  let t = parse {| (preprocess no_preprocessing) |} in
  (* Enforcing [no_preprocessing] does nothing if it is already set. *)
  enforce t [ no_preprocessing ];
  [%expect {| (preprocess no_preprocessing) |}];
  (* Enforcing the negation of [no_preprocessing] triggers an error. *)
  require_does_raise [%here] (fun () -> enforce t [ not_ no_preprocessing ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (not no_preprocessing)))
    |}];
  (* Blang. *)
  let t = parse {| (preprocess (pps ppx_sexp_conv)) |} in
  enforce t [ true_ ];
  [%expect {| (preprocess (pps ppx_sexp_conv)) |}];
  require_does_raise [%here] (fun () -> enforce t [ false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc       _)
      (condition false))
    |}];
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
  require_does_raise [%here] (fun () ->
    enforce t [ or_ [ pps (pp (Dune.Pp.Name.v "ppx_absent")); no_preprocessing ] ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
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
