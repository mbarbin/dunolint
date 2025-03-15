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
      let t = Dune_linter.Executable.read ~sexps_rewriter ~field in
      print_s (Dune_linter.Executable.write t))
  in
  test {| (executable (name main)) |};
  [%expect {| (executable (name main)) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [executable] field.
    [123]
    |}];
  test {| (executable (ignored field)) |};
  [%expect {| (executable) |}];
  test {| (executable (ignored field) and-more) |};
  [%expect {| (executable) |}];
  test {| (executable (name (invalid field))) |};
  [%expect
    {|
    Internal Error:
    (Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp (invalid field)))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let sexps_rewriter, field = Common.read {| (executable (name main)) |} in
  let t = Dune_linter.Executable.read ~sexps_rewriter ~field in
  print_s [%sexp (t : Dune_linter.Executable.t)];
  [%expect
    {|
    ((name (((name main))))
     (public_name ())
     (flags ((flags ())))
     (libraries ((sections (((entries ()))))))
     (instrumentation    ())
     (lint               ())
     (preprocess         ())
     (marked_for_removal ()))
    |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Executable.Predicate.t as 'a
    constraint
      'a =
      [ `name of Dune.Executable.Name.Predicate.t Blang.t
      | `public_name of Dune.Executable.Public_name.Predicate.t Blang.t
      | `lint of Dune.Lint.Predicate.t Blang.t
      | `instrumentation of Dune.Instrumentation.Predicate.t Blang.t
      | `preprocess of Dune.Preprocess.Predicate.t Blang.t
      | `has_field of [ `name | `public_name | `lint | `instrumentation | `preprocess ]
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let parse str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Executable.read ~sexps_rewriter ~field
  in
  let is_true b = require_equal [%here] (module Dunolint.Trilang) b True in
  let is_false b = require_equal [%here] (module Dunolint.Trilang) b False in
  let is_undefined b = require_equal [%here] (module Dunolint.Trilang) b Undefined in
  let t = parse {| (executable (name main)) |} in
  is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`name (equals (Dune.Executable.Name.v "main"))));
  [%expect {||}];
  is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`name (equals (Dune.Executable.Name.v "not-main"))));
  [%expect {||}];
  is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "my-cli"))));
  [%expect {||}];
  let t = parse {| (executable (name main) (public_name my-cli)) |} in
  is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "my-cli"))));
  [%expect {||}];
  is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "main"))));
  [%expect {||}];
  is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  let t =
    parse
      {|
(executable
 (name main)
 (public_name my-cli)
 (lint (pps ppx_linter -lint-flag)))
|}
  in
  is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_absent")))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let parse str =
    let sexps_rewriter, field = Common.read str in
    let t = Dune_linter.Executable.read ~sexps_rewriter ~field in
    sexps_rewriter, field, t
  in
  let enforce (sexps_rewriter, field, t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition ->
        Dune_linter.Executable.enforce t ~condition);
      Dune_linter.Executable.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
  let open! Blang.O in
  let t = parse {| (executable) |} in
  enforce t [];
  [%expect {| (executable) |}];
  let t = parse {| (executable (name main)) |} in
  enforce t [];
  [%expect {| (executable (name main)) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ name (equals (Dune.Executable.Name.v "main")) ];
  [%expect {| (executable (name main)) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ name (equals (Dune.Executable.Name.v "better-name")) ];
  [%expect {| (executable (name better-name)) |}];
  let t = parse {| (executable (name main)) |} in
  (* Enforcing the non-equality with another value has no effect. *)
  enforce t [ name (not_ (equals (Dune.Executable.Name.v "not_equal"))) ];
  [%expect {| (executable (name main)) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new name, this
     requires the user's intervention. *)
  require_does_raise [%here] (fun () ->
    enforce t [ name (not_ (equals (Dune.Executable.Name.v "main"))) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals main)))) |}];
  (* When there is no public_name, enforcing the equality with a value
     results in dunolint adding a new public_name field. *)
  let t = parse {| (executable (name main)) |} in
  enforce t [ public_name (equals (Dune.Executable.Public_name.v "my-cli")) ];
  [%expect
    {|
    (executable
      (name        main)
      (public_name my-cli))
    |}];
  let t = parse {| (executable (name main)) |} in
  (* When the require invariant is negated, and there is no
     public_name, this currently fails. This is questionable, perhaps
     the behavior is not yet very consistent in dunolint, and in other
     places dunolint simply does nothing when encountering undefined
     invariants. This may be revisited at some point, TBD, kept as
     characterization tests for now. *)
  require_does_raise [%here] (fun () ->
    enforce t [ public_name (not_ (equals (Dune.Executable.Public_name.v "my-cli"))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (public_name (not (equals my-cli)))))
    |}];
  ()
;;

let%expect_test "load_existing_libraries" =
  (* This is covering a use-case which hopefully will be deprecated in
     the future. Some external tool is using dunolint in such a way
     that existing fields are linted against values obtained with
     [create], which ends up erasing fields. *)
  let test t str ~load_existing_libraries =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Executable.Private.rewrite
      t
      ~load_existing_libraries
      ~sexps_rewriter
      ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Executable.create
      ~name:(Dune.Executable.Name.v "main")
      ~public_name:(Dune.Executable.Public_name.v "my-cli")
      ~libraries:[ Dune.Library.Name.v "foo"; Dune.Library.Name.v "bar" ]
      ()
  in
  let spec =
    {|
(executable
 (name other-name)
 (libraries baz))
|}
  in
  (* Here is the issue with this use-case. If we do not take special care to
     load existing libraries, they are removed, because they are not declared in
     [t]. See below, this is shown by the presence/absence of the [baz] library.

     The main use case that we aim to support eventually is linting via
     invariants, and usually, an invariant specify for a library to be present
     or absent, but another library that is not directly involved with said
     invariant is untouched when the invariant is enforced (not removed). *)
  test t spec ~load_existing_libraries:false;
  [%expect
    {|
    (executable
      (name        main)
      (public_name my-cli)
      (libraries bar foo))
    |}];
  test t spec ~load_existing_libraries:true;
  [%expect
    {|
    (executable
      (name        main)
      (public_name my-cli)
      (libraries bar baz foo))
    |}];
  ()
;;
