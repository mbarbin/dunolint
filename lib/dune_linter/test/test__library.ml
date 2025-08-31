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
  (* We exercise the case where flags have inner sexps. Maybe we'll drop this at
     some future point as this may not be needed in dune. *)
  rewrite {| (library (name main) (flags :standard (a sexp))) |};
  [%expect {| (library (name main) (flags :standard (a sexp))) |}];
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
  Test_helpers.is_undefined
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Library.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Library.eval t ~predicate:(`preprocess no_preprocessing));
  [%expect {||}];
  let _, t =
    parse
      {|
(library
 (name mylib)
 (public_name my-public-lib)
 (modes byte native)
 (instrumentation (backend bisect_ppx))
 (lint (pps ppx_linter -lint-flag))
 (preprocess no_preprocessing))
|}
  in
  (* Test condition evaluation with present fields. *)
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.Name.v "coverage"))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Library.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_absent")))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Library.eval t ~predicate:(`preprocess no_preprocessing));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare")))));
  [%expect {||}];
  List.iter
    [ `instrumentation; `lint; `modes; `name; `preprocess; `public_name ]
    ~f:(fun field ->
      Test_helpers.is_true (Dune_linter.Library.eval t ~predicate:(`has_field field)));
  [%expect {||}];
  let _, t_minimal = parse {| (library (name mylib)) |} in
  Test_helpers.is_true (Dune_linter.Library.eval t_minimal ~predicate:(`has_field `name));
  [%expect {||}];
  List.iter
    [ `instrumentation; `lint; `preprocess; `public_name; `modes ]
    ~f:(fun field ->
      Test_helpers.is_false
        (Dune_linter.Library.eval t_minimal ~predicate:(`has_field field)));
  [%expect {||}];
  ()
;;

let enforce_internal ((sexps_rewriter, field), t) conditions =
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
    Dune_linter.Library.rewrite t ~sexps_rewriter ~field)
;;

let enforce (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  enforce_internal input conditions;
  print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
;;

let enforce_diff (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  let original =
    Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn
  in
  enforce_internal input conditions;
  let changed =
    Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn
  in
  Expect_test_patdiff.print_patdiff_s original changed
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

let%expect_test "add_name_via_enforce" =
  (* This test covers helpers dedicated to finding initial values for field when
     an absent field is subject to invariants. Typically the function
     [Dunolinter.at_positive_enforcing_position] and its downstream usages. *)
  let init = {| (library (public_name my-cli)) |} in
  let main = Dune.Library.Name.v "main" in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  (* That's the easy case: you can simply pick the name from the invariant
     directly. *)
  test [ name (equals main) ];
  [%expect
    {|
    -1,1 +1,3
    -|(library (public_name my-cli))
    +|(library
    +|  (public_name my-cli)
    +|  (name        main))
    |}];
  (* The invariant will be undefined if the field isn't there. *)
  test [ name (is_prefix "hey") ];
  [%expect {||}];
  test [ name (is_suffix "hey") ];
  [%expect {||}];
  (* When the invariant contains an initial value, it is used to initialize the field. *)
  test [ name (and_ [ equals main; is_prefix "ma" ]) ];
  [%expect
    {|
    -1,1 +1,3
    -|(library (public_name my-cli))
    +|(library
    +|  (public_name my-cli)
    +|  (name        main))
    |}];
  (* Currently the application of invariant is not idempotent. See how, at the
     end of the application of this chain of [and_] the invariant no longer
     holds. We'll probably revisit at some later point, keeping as
     characterization tests for now. *)
  test [ name (and_ [ equals main; is_prefix "hey_" ]) ];
  [%expect
    {|
    -1,1 +1,3
    -|(library (public_name my-cli))
    +|(library
    +|  (public_name my-cli)
    +|  (name        hey_main))
    |}];
  (* When going through other blang constructs, currently we do not pick initial
     values. *)
  test [ name (or_ [ equals main; is_prefix "hey" ]) ];
  [%expect {||}];
  test [ name (if_ (is_prefix "hey") (is_suffix "ho") (equals main)) ];
  [%expect {||}];
  test [ name (not_ (equals main)) ];
  [%expect {||}];
  test [ name false_ ];
  [%expect {||}];
  let init = {| (library (name my_lib)) |} in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  test [ public_name (equals (Dune.Library.Public_name.v "public_lib")) ];
  [%expect
    {|
    -1,1 +1,3
    -|(library (name my_lib))
    +|(library
    +|  (name        my_lib)
    +|  (public_name public_lib))
    |}];
  test [ public_name (is_prefix "prefix_") ];
  [%expect {||}];
  test [ public_name (is_suffix "_suffix") ];
  [%expect {||}];
  ()
;;

let%expect_test "enforce_failures" =
  (* This covers cases yielding enforce failures. *)
  let init = {| (library) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  (* Certain fields don't have heuristics in place for initializing a value if
     it isn't there. *)
  require_does_raise [%here] (fun () -> test [ has_field `name ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (has_field name))) |}];
  require_does_raise [%here] (fun () -> test [ has_field `public_name ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure
      (loc _)
      (condition (has_field public_name)))
    |}];
  ()
;;

let%expect_test "undefined conditions" =
  let init = {| (library (public_name my-cli)) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  let main = Dune.Library.Name.v "main" in
  let _, t = parse init in
  Test_helpers.is_undefined
    (Dune_linter.Library.eval t ~predicate:(`name (is_prefix "hey")));
  [%expect {||}];
  (* When a condition is undefined, the entire if-then-else is ignored. *)
  test [ if_ (name (is_prefix "hey")) (name (equals main)) (name (is_suffix "ho")) ];
  [%expect {| (library (public_name my-cli)) |}];
  (* Beware of static code simplifications performed by Blang though! In the
     following example, the [if_] is rewritten as a [And _] sequence, with the
     first item being unapplicable, and thus ignored, and the second one being
     evaluated. Arguably this is quite surprising, and maybe the semantic of
     [And] shall be revisited. Left as characterization test for future work. *)
  test [ if_ (name (is_prefix "hey")) (name (equals main)) false_ ];
  [%expect
    {|
    (library
      (public_name my-cli)
      (name        main))
    |}];
  ()
;;

let%expect_test "non base negations" =
  let init = {| (library (public_name my-cli)) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  test [ not_ (name (or_ [ is_prefix "hey"; is_suffix "ho" ])) ];
  [%expect {| (library (public_name my-cli)) |}];
  test [ not_ (or_ [ name (is_prefix "hey"); name (is_suffix "ho") ]) ];
  [%expect {| (library (public_name my-cli)) |}];
  ()
;;

let%expect_test "has_field_auto_initialize" =
  (* Test fields that can be auto-initialized when missing. *)
  let init = {| (library (name my-lib)) |} in
  (* [instrumentation] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `instrumentation ];
  [%expect {| (library (name my-lib) (instrumentation (backend bisect_ppx))) |}];
  (* [lint] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `lint ];
  [%expect {| (library (name my-lib) (lint (pps))) |}];
  (* [preprocess] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `preprocess ];
  [%expect
    {|
    (library
      (name       my-lib)
      (preprocess no_preprocessing))
    |}];
  (* [modes] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `modes ];
  [%expect
    {|
    (library
      (name  my-lib)
      (modes best))
    |}];
  ()
;;

let%expect_test "field_conditions" =
  (* Test field-specific condition enforcement. *)
  let init = {| (library (name my-lib)) |} in
  (* [instrumentation] condition auto-creates field *)
  let t = parse init in
  enforce
    t
    [ instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")) ];
  [%expect {| (library (name my-lib) (instrumentation (backend bisect_ppx))) |}];
  (* [lint] condition auto-creates field. *)
  let t = parse init in
  enforce t [ lint (pps (pp (Dune.Pp.Name.v "ppx_linter"))) ];
  [%expect {| (library (name my-lib) (lint (pps ppx_linter))) |}];
  (* [preprocess] condition auto-creates field. *)
  let t = parse init in
  enforce t [ preprocess no_preprocessing ];
  [%expect
    {|
    (library
      (name       my-lib)
      (preprocess no_preprocessing))
    |}];
  (* [modes] condition auto-creates field. *)
  let t = parse init in
  enforce t [ modes (has_mode `byte) ];
  [%expect
    {|
    (library
      (name  my-lib)
      (modes byte))
    |}];
  ()
;;

let%expect_test "remove_fields" =
  (* Test removing fields via [not has_field]. *)
  let init =
    {|
(library
 (name my-lib)
 (public_name my-public-lib)
 (modes byte native)
 (instrumentation (backend bisect_ppx))
 (lint (pps ppx_linter))
 (preprocess no_preprocessing))
|}
  in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  test [ not_ (has_field `instrumentation) ];
  [%expect
    {|
    -1,7 +1,6
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
    -|  (instrumentation (backend bisect_ppx))
        (lint (pps ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `lint) ];
  [%expect
    {|
    -1,7 +1,6
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
    -|  (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `preprocess) ];
  [%expect
    {|
    -1,7 +1,6
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
    -|  (lint            (pps     ppx_linter))
    -|  (preprocess no_preprocessing))
    +|  (lint            (pps     ppx_linter)))
    |}];
  test [ not_ (has_field `modes) ];
  [%expect
    {|
    -1,7 +1,6
      (library
        (name        my-lib)
        (public_name my-public-lib)
    -|  (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `name) ];
  [%expect
    {|
    -1,7 +1,6
      (library
    -|  (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `public_name) ];
  [%expect
    {|
    -1,7 +1,6
      (library
        (name my-lib)
    -|  (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  ()
;;

let%expect_test "positive_enforcement_with_existing_fields" =
  (* Test enforcement conditions when fields are already present. *)
  let init =
    {|
(library
 (name my-lib)
 (public_name my-public-lib)
 (modes byte native)
 (instrumentation (backend bisect_ppx))
 (lint (pps ppx_linter))
 (preprocess no_preprocessing))
|}
  in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  test [ has_field `name ];
  [%expect {||}];
  test [ has_field `public_name ];
  [%expect {||}];
  test [ has_field `modes ];
  [%expect {||}];
  test [ has_field `instrumentation ];
  [%expect {||}];
  test [ has_field `lint ];
  [%expect {||}];
  test [ has_field `preprocess ];
  [%expect {||}];
  ()
;;

let%expect_test "field_condition_enforcement_with_existing_fields" =
  (* Test fields condition enforcement when fields are already present. *)
  let init =
    {|
(library
 (name my-lib)
 (public_name my-public-lib)
 (modes byte native)
 (instrumentation (backend bisect_ppx))
 (lint (pps ppx_linter))
 (preprocess no_preprocessing))
|}
  in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  test [ public_name (equals (Dune.Library.Public_name.v "new.name")) ];
  [%expect
    {|
    -1,7 +1,7
      (library
        (name        my-lib)
    -|  (public_name my-public-lib)
    +|  (public_name new.name)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ public_name (is_prefix "lib."); public_name (is_suffix "-pub") ];
  [%expect
    {|
    -1,7 +1,7
      (library
        (name        my-lib)
    -|  (public_name my-public-lib)
    +|  (public_name lib.my-public-lib-pub)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ instrumentation (backend (Dune.Instrumentation.Backend.Name.v "coverage")) ];
  [%expect
    {|
    -1,7 +1,7
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
    -|  (instrumentation (backend bisect_ppx))
    +|  (instrumentation (backend coverage))
        (lint            (pps     ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ lint (pps (pp (Dune.Pp.Name.v "ppx_deriving"))) ];
  [%expect
    {|
    -1,7 +1,7
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
    -|  (lint            (pps     ppx_linter))
    +|  (lint (pps ppx_deriving ppx_linter))
        (preprocess no_preprocessing))
    |}];
  test [ preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare"))) ];
  [%expect
    {|
    -1,7 +1,7
      (library
        (name        my-lib)
        (public_name my-public-lib)
        (modes byte native)
        (instrumentation (backend bisect_ppx))
        (lint            (pps     ppx_linter))
    -|  (preprocess no_preprocessing))
    +|  (preprocess      (pps     ppx_compare)))
    |}];
  ()
;;
