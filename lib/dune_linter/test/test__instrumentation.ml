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
      let t =
        try Dune_linter.Instrumentation.read ~sexps_rewriter ~field with
        | Sexp.Of_sexp_error (_, sexp) ->
          (* We redact the message because it is contains paths to source files, which
             makes it inconvenient when relocating the code in sub repos. *)
          raise_s
            [%sexp Of_sexp_error, ("_", { invalid_sexp = (sexp : Sexp.t) })]
          [@coverage off]
      in
      print_s (Dune_linter.Instrumentation.write t))
  in
  test {| (instrumentation (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  test {| (instrumentation) |};
  [%expect
    {|
    File "dune", line 1, characters 1-18:
    Error: Required [backend] value in instrumentation.
    [123]
    |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [instrumentation] field.
    [123]
    |}];
  test {| (instrumentation (backend other_backend)) |};
  [%expect {| (instrumentation (backend other_backend)) |}];
  (* Dunolint simply ignores other fields if any. *)
  test {| (instrumentation (other field) (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  ()
;;

let%expect_test "sexp_of" =
  let sexps_rewriter, field = Common.read {| (instrumentation (backend bisect_ppx)) |} in
  let t = Dune_linter.Instrumentation.read ~sexps_rewriter ~field in
  print_s [%sexp (t : Dune_linter.Instrumentation.t)];
  [%expect {| ((backend bisect_ppx)) |}];
  ()
;;

let parse str =
  let sexps_rewriter, field = Common.read str in
  let t = Dune_linter.Instrumentation.read ~sexps_rewriter ~field in
  sexps_rewriter, field, t
;;

let rewrite ?(f = ignore) str =
  let sexps_rewriter, field, t = parse str in
  f t;
  Dune_linter.Instrumentation.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (instrumentation (backend bisect_ppx)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Exercising some getters. *)
  rewrite {| (instrumentation (backend other_backend)) |} ~f:(fun t ->
    print_s
      [%sexp
        (Dune_linter.Instrumentation.backend t : Dune.Instrumentation.Backend.Name.t)];
    [%expect {| other_backend |}];
    ());
  [%expect {| (instrumentation (backend other_backend)) |}];
  (* Exercising some setters. *)
  rewrite {| (instrumentation (backend other_backend)) |} ~f:(fun t ->
    Dune_linter.Instrumentation.set_backend
      t
      ~backend:(Dune.Instrumentation.Backend.Name.v "bisect_ppx");
    ());
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Instrumentation.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Instrumentation.create
      ~backend:(Dune.Instrumentation.Backend.Name.v "bisect_ppx")
  in
  test t {| (instrumentation (backend other_backend)) |};
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Rewrite currently will do nothing if the stanza to rewrite doesn't have a
     backend. Maybe this is not right? Keeping as characterization test. *)
  test t {| (instrumentation (other field)) |};
  [%expect {| (instrumentation (other field)) |}];
  (* As long as the targeted field is present, it is rewritten. *)
  test t {| (instrumentation (other field) (backend other_backend)) |};
  [%expect
    {|
    (instrumentation
      (other   field)
      (backend bisect_ppx))
    |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Instrumentation.Predicate.t as 'a
    constraint 'a = [ `backend of Dune.Instrumentation.Backend.Name.t ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let parse str =
    let _, _, t = parse str in
    t
  in
  let t = parse {| (instrumentation (backend bisect_ppx)) |} in
  let is_true b = require_equal [%here] (module Dunolint.Trilang) b True in
  let is_false b = require_equal [%here] (module Dunolint.Trilang) b False in
  is_true
    (Dune_linter.Instrumentation.eval
       t
       ~predicate:(`backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")));
  [%expect {| |}];
  is_false
    (Dune_linter.Instrumentation.eval
       t
       ~predicate:(`backend (Dune.Instrumentation.Backend.Name.v "other_backend")));
  [%expect {| |}];
  ()
;;

let%expect_test "enforce" =
  let enforce (sexps_rewriter, field, t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Instrumentation.enforce t ~condition);
      Dune_linter.Instrumentation.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open Blang.O in
  let t = parse {| (instrumentation (backend bisect_ppx)) |} in
  enforce t [];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx") ];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ backend (Dune.Instrumentation.Backend.Name.v "other_backend") ];
  [%expect {| (instrumentation (backend other_backend)) |}];
  let t = parse {| (instrumentation (backend other_backend)) |} in
  (* Enforcing the negation of the equality with another value has no effect. *)
  enforce t [ not_ (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")) ];
  [%expect {| (instrumentation (backend other_backend)) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new setting, this
     requires the user's intervention. *)
  require_does_raise [%here] (fun () ->
    enforce t [ not_ (backend (Dune.Instrumentation.Backend.Name.v "other_backend")) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (not (backend other_backend))))
    |}];
  (* Blang. *)
  let t = parse {| (instrumentation (backend bisect_ppx)) |} in
  enforce t [ true_ ];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
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
        [ not_ (backend (Dune.Instrumentation.Backend.Name.v "other_backend"))
        ; backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")
        ]
    ];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  (* [or] does not have an enforcement strategy when its invariant is
     not satisfied. *)
  enforce
    t
    [ or_
        [ backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")
        ; backend (Dune.Instrumentation.Backend.Name.v "other_backend")
        ]
    ];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  require_does_raise [%here] (fun () ->
    enforce
      t
      [ or_
          [ backend (Dune.Instrumentation.Backend.Name.v "qualified")
          ; backend (Dune.Instrumentation.Backend.Name.v "no")
          ]
      ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (
        or
        (backend qualified)
        (backend no))))
    |}];
  (* When defined, [if] enforces the clause that applies. *)
  let invariant =
    if_
      (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))
      (backend (Dune.Instrumentation.Backend.Name.v "other_backend"))
      (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))
  in
  let t = parse {| (instrumentation (backend bisect_ppx)) |} in
  enforce t [ invariant ];
  [%expect {| (instrumentation (backend other_backend)) |}];
  let t = parse {| (instrumentation (backend other_backend)) |} in
  enforce t [ invariant ];
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  ()
;;

let%expect_test "initialize" =
  let test condition =
    let t = Dune_linter.Instrumentation.initialize ~condition in
    print_s (Dune_linter.Instrumentation.write t)
  in
  test true_;
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  test (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"));
  [%expect {| (instrumentation (backend bisect_ppx)) |}];
  ()
;;
