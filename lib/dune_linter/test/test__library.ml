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
  Test_helpers.parse (module Dune_linter.Library) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
      print_s (Dune_linter.Library.write t))
  in
  test {| (library (name mylib)) |};
  [%expect {| (library (name mylib)) |}];
  test {| (invalid field) |};
  [%expect
    {|
    File "dune", line 1, characters 1-16:
    Error: Unexpected [library] field.
    [123]
    |}];
  test {| (library (name (invalid field))) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (invalid field)))))
    <backtrace disabled in tests>
    [125]
    |}];
  test {| (library (ignored field)) |};
  [%expect {| (library) |}];
  test {| (library (ignored field) and-more) |};
  [%expect {| (library) |}];
  test {| (library (name my_test) (inline_tests)) |};
  [%expect {| (library (name my_test) (inline_tests)) |}];
  test
    {|
(library
 (name dune_linter)
 (public_name dunolint.dune_linter)
 (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Base)
 (libraries base dunolint-lib dunolinter etc)
 (instrumentation
  (backend bisect_ppx))
 (lint
  (pps ppx_js_style -allow-let-operators -check-doc-comments))
 (preprocess
  (pps
   -unused-code-warnings=force
   ppx_compare
   ppx_enumerate
   and_more)))
|};
  [%expect
    {|
    (library
      (name        dune_linter)
      (public_name dunolint.dune_linter)
      (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Base)
      (libraries base dunolint-lib dunolinter etc)
      (instrumentation (backend bisect_ppx))
      (lint (pps ppx_js_style -allow-let-operators -check-doc-comments))
      (preprocess (
        pps -unused-code-warnings=force ppx_compare ppx_enumerate and_more)))
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (library (name mylib)) |} in
  print_s [%sexp (t : Dune_linter.Library.t)];
  [%expect
    {|
    ((name (((name mylib))))
     (public_name  ())
     (inline_tests ())
     (modes        ())
     (flags     ((flags    ())))
     (libraries ((sections ())))
     (libraries_to_open_via_flags ())
     (instrumentation             ())
     (lint                        ())
     (preprocess                  ())
     (marked_for_removal          ()))
    |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  let spec =
    {|
(library
 (name dune_linter)
 (public_name dunolint.dune_linter)
 (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Base)
 (libraries base dunolint-lib dunolinter etc)
 (instrumentation
  (backend bisect_ppx))
 (lint
  (pps ppx_js_style -allow-let-operators -check-doc-comments))
 (unknown_field blah)
 (preprocess
  (pps
   -unused-code-warnings=force
   ppx_compare
   ppx_enumerate
   and_more)))
|}
  in
  rewrite spec;
  [%expect
    {|
    (library
     (name dune_linter)
     (public_name dunolint.dune_linter)
     (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Base)
     (libraries base dunolint-lib dunolinter etc)
     (instrumentation
      (backend bisect_ppx))
     (lint
      (pps ppx_js_style -allow-let-operators -check-doc-comments))
     (unknown_field blah)
     (preprocess
      (pps
       -unused-code-warnings=force
       ppx_compare
       ppx_enumerate
       and_more)))
    |}];
  rewrite spec ~f:(fun t ->
    let open Dunolint.Config.Std in
    Dune_linter.Library.enforce
      t
      ~condition:
        (and_ [ not_ (has_field `instrumentation); not_ (has_field `preprocess) ]));
  [%expect
    {|
    (library
     (name dune_linter)
     (public_name dunolint.dune_linter)
     (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a -open Base)
     (libraries base dunolint-lib dunolinter etc)

     (lint
      (pps ppx_js_style -allow-let-operators -check-doc-comments))
     (unknown_field blah)
     )
    |}];
  rewrite {| (library) |};
  [%expect {| (library) |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Library.create () in
  test t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  let t =
    Dune_linter.Library.create
      ~public_name:(Dune.Library.Public_name.v "my-lib")
      ~inline_tests:false
      ~libraries:[ Dune.Library.Name.v "foo"; Dune.Library.Name.v "my-dep" ]
      ~modes:(Dunolinter.Ordered_set.of_list [ `byte; `native ])
      ~libraries_to_open_via_flags:[ "my-dep"; "other" ]
      ()
  in
  test t {| (library (name main)) |};
  [%expect
    {|
    (library
      (name        main)
      (public_name my-lib)
      (modes     byte  native)
      (flags     -open My_dep)
      (libraries foo   my-dep))
    |}];
  (* Inline tests is optional, when not specified the current value is
     ignored. *)
  let t = Dune_linter.Library.create () in
  test t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  test t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  let t = Dune_linter.Library.create ~inline_tests:true () in
  test t {| (library (name main)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  test t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main) (inline_tests)) |}];
  let t = Dune_linter.Library.create ~inline_tests:false () in
  test t {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  test t {| (library (name main) (inline_tests)) |};
  [%expect {| (library (name main)) |}];
  ()
;;

let%expect_test "libraries_to_open_via_flags" =
  (* This part extends the testing dedicated to that feature. *)
  let test ~libraries_to_open_via_flags str =
    let sexps_rewriter, field = Common.read str in
    let fields =
      Dunolinter.Sexp_handler.get_args
        ~field_name:Dune_linter.Library.field_name
        ~sexps_rewriter
        ~field
    in
    let existing_flags =
      Dunolinter.Sexp_handler.find (module Dune_linter.Flags) ~sexps_rewriter ~fields
      |> Option.value_map ~default:[] ~f:Dune_linter.Flags.flags
    in
    let existing_libraries =
      Dunolinter.Sexp_handler.find (module Dune_linter.Libraries) ~sexps_rewriter ~fields
      |> Option.value_map ~default:[] ~f:Dune_linter.Libraries.entries
      |> List.filter_map ~f:Dune_linter.Libraries.Entry.library_name
    in
    let t =
      Dune_linter.Library.create
        ~flags:existing_flags
        ~libraries:existing_libraries
        ~libraries_to_open_via_flags
        ()
    in
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  test ~libraries_to_open_via_flags:[ "a" ] {| (library (name main)) |};
  [%expect {| (library (name main)) |}];
  test
    ~libraries_to_open_via_flags:[ "a" ]
    {|
      (library
       (name main)
       (libraries a))
    |};
  [%expect {| (library (name main) (flags -open A) (libraries a)) |}];
  test
    ~libraries_to_open_via_flags:[ "a" ]
    {|
      (library
       (name main)
       (flags -open A)
       (libraries a))
    |};
  [%expect {| (library (name main) (flags -open A) (libraries a)) |}];
  test
    ~libraries_to_open_via_flags:[ "b"; "a"; "c" ]
    {|
      (library
       (name main)
       (libraries a b c))
    |};
  [%expect {| (library (name main) (flags -open B -open A -open C) (libraries a b c)) |}];
  test
    ~libraries_to_open_via_flags:[ "b"; "a"; "c" ]
    {|
      (library
       (name main)
       (flags -hey -open B -open A -open C)
       (libraries a b c))
    |};
  [%expect
    {| (library (name main) (flags -hey -open B -open A -open C) (libraries a b c)) |}];
  test
    ~libraries_to_open_via_flags:[ "b"; "a"; "c" ]
    {|
      (library
       (name main)
       (flags -hey -open A)
       (libraries a b c))
    |};
  [%expect
    {| (library (name main) (flags -hey -open B -open A -open C) (libraries a b c)) |}];
  test
    ~libraries_to_open_via_flags:[ "b"; "a"; "c" ]
    {|
      (library
       (name main)
       (flags -hey -open A -open B -open C)
       (libraries a b c))
    |};
  [%expect
    {| (library (name main) (flags -hey -open B -open A -open C) (libraries a b c)) |}];
  test
    ~libraries_to_open_via_flags:[ "b"; "a"; "c" ]
    {|
      (library
       (name main)
       (flags -hey -open A -open Other -open C -open B)
       (libraries a b c))
    |};
  [%expect
    {|
    (library
      (name main)
      (flags -hey -open A -open Other -open B -open C)
      (libraries a b c))
    |}];
  ()
;;

module Predicate = struct
  (* Aliased here so we remember to add new tests when this type is modified. *)
  type t = Dune.Library.Predicate.t as 'a
    constraint
      'a =
      [ `has_field of
          [ `instrumentation | `lint | `modes | `name | `preprocess | `public_name ]
      | `instrumentation of Dune.Instrumentation.Predicate.t Blang.t
      | `lint of Dune.Lint.Predicate.t Blang.t
      | `modes of Dune.Library.Modes.Predicate.t Blang.t
      | `name of Dune.Library.Name.Predicate.t Blang.t
      | `preprocess of Dune.Preprocess.Predicate.t Blang.t
      | `public_name of Dune.Library.Public_name.Predicate.t Blang.t
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (library (name mylib)) |} in
  Test_helpers.is_true
    (Dune_linter.Library.eval t ~predicate:(`name (equals (Dune.Library.Name.v "mylib"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`name (equals (Dune.Library.Name.v "not-mylib"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Library.eval
       t
       ~predicate:(`public_name (equals (Dune.Library.Public_name.v "my-public-lib"))));
  [%expect {||}];
  let _, t = parse {| (library (name mylib) (public_name my-public-lib)) |} in
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:(`public_name (equals (Dune.Library.Public_name.v "my-public-lib"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`public_name (equals (Dune.Library.Public_name.v "mylib"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Library.eval t ~predicate:(`modes (has_mode `best)));
  [%expect {||}];
  let _, t =
    parse
      {|
(library
 (name mylib)
 (public_name my-public-lib)
 (modes byte native))
|}
  in
  Test_helpers.is_true
    (Dune_linter.Library.eval t ~predicate:(`modes (has_modes [ `byte; `native ])));
  [%expect {||}];
  Test_helpers.is_false (Dune_linter.Library.eval t ~predicate:(`modes (has_mode `best)));
  [%expect {||}];
  ()
;;

let enforce ((sexps_rewriter, field), t) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let open! Blang.O in
  let t = parse {| (library) |} in
  enforce t [];
  [%expect {| (library) |}];
  let t = parse {| (library (name mylib)) |} in
  enforce t [];
  [%expect {| (library (name mylib)) |}];
  (* Enforcing the equality with the current value has no effect. *)
  enforce t [ name (equals (Dune.Library.Name.v "mylib")) ];
  [%expect {| (library (name mylib)) |}];
  (* Enforcing the equality with a new value changes it. *)
  enforce t [ name (equals (Dune.Library.Name.v "better-name")) ];
  [%expect {| (library (name better-name)) |}];
  let t = parse {| (library (name mylib)) |} in
  (* Enforcing the non-equality with another value has no effect. *)
  enforce t [ name (not_ (equals (Dune.Library.Name.v "not_equal"))) ];
  [%expect {| (library (name mylib)) |}];
  (* Enforcing the negation of a current equality triggers an error.
     Dunolint is not going to automatically invent a new name, this
     requires the user's intervention. *)
  require_does_raise [%here] (fun () ->
    enforce t [ name (not_ (equals (Dune.Library.Name.v "mylib"))) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals mylib)))) |}];
  (* When there is no public_name, enforcing the equality with a value results
     in dunolint adding a new public_name field. *)
  let t = parse {| (library (name mylib)) |} in
  enforce t [ public_name (equals (Dune.Library.Public_name.v "my-public-lib")) ];
  [%expect
    {|
    (library
      (name        mylib)
      (public_name my-public-lib))
    |}];
  let t = parse {| (library (name mylib)) |} in
  (* When the required invariant is negated, and there is no public_name,
     dunolint simply does nothing and considers it an undefined invariants. *)
  enforce t [ public_name (not_ (equals (Dune.Library.Public_name.v "my-public-lib"))) ];
  [%expect {| (library (name mylib)) |}];
  (* When there is no [modes], enforcing a invariant about this field results in
     dunolint creating a new field. *)
  let t = parse {| (library (name mylib)) |} in
  enforce t [ modes (has_mode `native) ];
  [%expect
    {|
    (library
      (name  mylib)
      (modes native))
    |}];
  (* Otherwise the mode is edited in place. *)
  let t = parse {| (library (name mylib) (modes byte)) |} in
  enforce t [ modes (has_mode `native) ];
  [%expect {| (library (name mylib) (modes byte native)) |}];
  (* Currently adding a field is only possible if some are already present. *)
  let t = parse {| (library) |} in
  require_does_raise [%here] (fun () ->
    enforce t [ name (equals (Dune.Library.Name.v "mylib")) ]);
  [%expect {| "Existing stanza in dune file expected to have at least one field." |}];
  ()
;;

let%expect_test "load_existing_libraries" =
  (* This is covering a use-case which hopefully will be deprecated in the
     future. Some external tool is using dunolint in such a way that existing
     fields are linted against values obtained with [create], which ends up
     erasing fields. *)
  let test t str ~load_existing_libraries =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Library.Private.rewrite t ~load_existing_libraries ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t =
    Dune_linter.Library.create
      ~name:(Dune.Library.Name.v "main")
      ~public_name:(Dune.Library.Public_name.v "my-cli")
      ~libraries:[ Dune.Library.Name.v "foo"; Dune.Library.Name.v "bar" ]
      ()
  in
  let spec =
    {|
(library
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
    (library
      (name        main)
      (public_name my-cli)
      (libraries bar foo))
    |}];
  test t spec ~load_existing_libraries:true;
  [%expect
    {|
    (library
      (name        main)
      (public_name my-cli)
      (libraries bar baz foo))
    |}];
  ()
;;
