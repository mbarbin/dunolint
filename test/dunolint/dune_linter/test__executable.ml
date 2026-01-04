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
  Test_helpers.parse (module Dune_linter.Executable) ~path:(Fpath.v "dune") contents
;;

let%expect_test "read/write" =
  let test contents =
    Err.For_test.protect (fun () ->
      let _, t = parse contents in
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
  test
    {|
(executable
 (name main)
 (public_name my-cli)
 (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a)
 (instrumentation
  (backend bisect_ppx))
 (lint (pps ppx_linter -lint-flag))
 (preprocess no_preprocessing))
|};
  [%expect
    {|
    (executable (name main) (public_name my-cli)
     (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a)
     (instrumentation (backend bisect_ppx)) (lint (pps ppx_linter -lint-flag))
     (preprocess no_preprocessing))
    |}];
  test {| (executable (name (invalid field))) |};
  [%expect
    {|
    Internal Error: (Of_sexp_error (_ ((invalid_sexp (invalid field)))))
    <backtrace disabled in tests>
    [125]
    |}];
  ()
;;

let%expect_test "sexp_of" =
  let _, t = parse {| (executable (name main)) |} in
  print_s [%sexp (t : Dune_linter.Executable.t)];
  [%expect {| ((name ((name main)))) |}];
  ()
;;

let rewrite ?(f = ignore) str =
  let (sexps_rewriter, field), t = parse str in
  f t;
  Dune_linter.Executable.rewrite t ~sexps_rewriter ~field;
  print_endline (Sexps_rewriter.contents sexps_rewriter)
;;

let%expect_test "rewrite" =
  rewrite {| (executable (name main)) |};
  [%expect {| (executable (name main)) |}];
  rewrite {| (executable (ignored field) and-more) |};
  [%expect {| (executable (ignored field) and-more) |}];
  rewrite
    {|
(executable
 (name main)
 (public_name my-cli)
 (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a)
 (instrumentation
  (backend bisect_ppx))
 (lint (pps ppx_linter -lint-flag))
 (preprocess no_preprocessing)
 (unknown_field blah))
|};
  [%expect
    {|
    (executable
     (name main)
     (public_name my-cli)
     (flags :standard -w +a-4-40-41-42-44-45-48-66 -warn-error +a)
     (instrumentation
      (backend bisect_ppx))
     (lint (pps ppx_linter -lint-flag))
     (preprocess no_preprocessing)
     (unknown_field blah))
    |}];
  rewrite
    {|
(executable
 (name main)
 (public_name my-cli)
 (instrumentation
  (backend bisect_ppx))
 (lint (pps ppx_linter -lint-flag))
 (preprocess no_preprocessing))
|}
    ~f:(fun t ->
      let open Dunolint.Config.Std in
      Dune_linter.Executable.enforce
        t
        ~condition:
          (and_ [ not_ (has_field `instrumentation); not_ (has_field `preprocess) ]));
  [%expect
    {|
    (executable
     (name main)
     (public_name my-cli)

     (lint (pps ppx_linter -lint-flag))
     )
    |}];
  ()
;;

let%expect_test "create_then_rewrite" =
  (* This covers some unusual cases. The common code path does not involve
     rewriting values that are created via [create]. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Executable.rewrite t ~sexps_rewriter ~field;
    print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
  in
  let t = Dune_linter.Executable.create () in
  test t {| (executable (name main)) |};
  [%expect {| (executable (name main)) |}];
  let t =
    Dune_linter.Executable.create
      ~public_name:(Dune.Executable.Public_name.v "my-cli")
      ~libraries:[ Dune.Library.Name.v "foo"; Dune.Library.Name.v "bar" ]
      ()
  in
  test t {| (executable (name main)) |};
  [%expect {| (executable (name main) (public_name my-cli) (libraries bar foo)) |}];
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
      | `has_field of [ `instrumentation | `lint | `name | `preprocess | `public_name ]
      ]
end

open Dunolint.Config.Std

let%expect_test "eval" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let _, t = parse {| (executable (name main)) |} in
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`name (equals (Dune.Executable.Name.v "main"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`name (equals (Dune.Executable.Name.v "not-main"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval t ~predicate:(`has_field `instrumentation));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "my-cli"))));
  [%expect {||}];
  let _, t = parse {| (executable (public_name my-cli)) |} in
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:(`name (equals (Dune.Executable.Name.v "not-main"))));
  [%expect {||}];
  let _, t = parse {| (executable (name main) (public_name my-cli)) |} in
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "my-cli"))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`public_name (equals (Dune.Executable.Public_name.v "main"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))));
  [%expect {||}];
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval t ~predicate:(`preprocess no_preprocessing));
  [%expect {||}];
  let _, t =
    parse
      {|
(executable
 (name main)
 (public_name my-cli)
 (instrumentation
  (backend bisect_ppx))
 (lint (pps ppx_linter -lint-flag))
 (preprocess no_preprocessing))
|}
  in
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_linter")))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Executable.eval
       t
       ~predicate:(`lint (pps (pp (Dune.Pp.Name.v "ppx_absent")))));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Executable.eval t ~predicate:(`preprocess no_preprocessing));
  [%expect {||}];
  Test_helpers.is_true
    (Dune_linter.Executable.eval
       t
       ~predicate:
         (`instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx"))));
  [%expect {||}];
  List.iter [ `instrumentation; `lint; `name; `preprocess; `public_name ] ~f:(fun field ->
    Test_helpers.is_true (Dune_linter.Executable.eval t ~predicate:(`has_field field)));
  [%expect {||}];
  ()
;;

let enforce_internal ((sexps_rewriter, field), t) conditions =
  Dunolinter.Handler.raise ~f:(fun () ->
    List.iter conditions ~f:(fun condition -> Dune_linter.Executable.enforce t ~condition);
    Dune_linter.Executable.rewrite t ~sexps_rewriter ~field)
;;

let enforce (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  enforce_internal input conditions;
  print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn)
;;

let enforce_diff (((sexps_rewriter, _), _) as input) conditions =
  Sexps_rewriter.reset sexps_rewriter;
  let original =
    Dunolint_engine.format_dune_file
      ~new_contents:(Sexps_rewriter.contents sexps_rewriter)
  in
  enforce_internal input conditions;
  let changed =
    Dunolint_engine.format_dune_file
      ~new_contents:(Sexps_rewriter.contents sexps_rewriter)
  in
  Expect_test_patdiff.print_patdiff original changed ~context:3
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
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
  require_does_raise (fun () ->
    enforce t [ name (not_ (equals (Dune.Executable.Name.v "main"))) ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (not (equals main)))) |}];
  (* When there is no public_name, enforcing the equality with a value results
     in dunolint adding a new public_name field. *)
  let t = parse {| (executable (name main)) |} in
  enforce t [ public_name (equals (Dune.Executable.Public_name.v "my-cli")) ];
  [%expect {| (executable (name main) (public_name my-cli)) |}];
  let t = parse {| (executable (name main)) |} in
  (* When the field is absent and the condition cannot provide an initial value
     (e.g., negation, is_prefix, is_suffix), enforcement fails. The user must
     add the field manually before dunolint can enforce constraints on it. *)
  require_does_raise (fun () ->
    enforce t [ public_name (not_ (equals (Dune.Executable.Public_name.v "my-cli"))) ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (public_name (not (equals my-cli)))))
    |}];
  ()
;;

let%expect_test "overwrite_existing_libraries" =
  (* This is covering a use-case which we deprecated. When you rewrite a
     library, the fields that were specified overwrite the ones found on disk,
     which may end up erasing fields. *)
  let test t str =
    let sexps_rewriter, field = Common.read str in
    Dune_linter.Executable.rewrite t ~sexps_rewriter ~field;
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

     We replaced this with linting via invariants, and/or linting via the typed
     linter API. *)
  test t spec;
  [%expect {| (executable (name main) (public_name my-cli) (libraries bar foo)) |}];
  ()
;;

let%expect_test "add_name_via_enforce" =
  (* This test covers the initialization of absent fields during enforcement.
     Only [equals] predicates at positive enforcing positions (Base and And)
     can provide initial values. Other predicates like [is_prefix] or [is_suffix]
     cannot provide initial values and cause enforcement to fail. *)
  let init = {| (executable (public_name my-cli)) |} in
  let main = Dune.Executable.Name.v "main" in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  let test_fails cond =
    let t = parse init in
    require_does_raise (fun () -> enforce t cond)
  in
  (* [equals] at a positive position can initialize the field. *)
  test [ name (equals main) ];
  [%expect
    {|
    -1,2 +1,3
      (executable
    -| (public_name my-cli))
    +| (public_name my-cli)
    +| (name main))
    |}];
  (* [is_prefix] and [is_suffix] cannot provide initial values - enforcement fails. *)
  test_fails [ name (is_prefix "hey") ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (is_prefix hey))))
    |}];
  test_fails [ name (is_suffix "hey") ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (is_suffix hey))))
    |}];
  (* When [equals] is combined with other predicates in [and_], the initial
     value from [equals] is used. *)
  test [ name (and_ [ equals main; is_prefix "ma" ]) ];
  [%expect
    {|
    -1,2 +1,3
      (executable
    -| (public_name my-cli))
    +| (public_name my-cli)
    +| (name main))
    |}];
  (* Currently the application of invariant is not idempotent. See how, at the
     end of the application of this chain of [and_] the invariant no longer
     holds. We'll probably revisit at some later point, keeping as
     characterization tests for now. *)
  test [ name (and_ [ equals main; is_prefix "hey_" ]) ];
  [%expect
    {|
    -1,2 +1,3
      (executable
    -| (public_name my-cli))
    +| (public_name my-cli)
    +| (name hey_main))
    |}];
  (* Predicates inside [or_], [if_], or [not_] are not at positive enforcing
     positions, so they cannot provide initial values - enforcement fails. *)
  test_fails [ name (or_ [ equals main; is_prefix "hey" ]) ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (or (equals main) (is_prefix hey)))))
    |}];
  test_fails [ name (if_ (is_prefix "hey") (is_suffix "ho") (equals main)) ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (if (is_prefix hey) (is_suffix ho) (equals main)))))
    |}];
  test_fails [ name (not_ (equals main)) ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (not (equals main)))))
    |}];
  test_fails [ name false_ ];
  [%expect {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (name false))) |}];
  ()
;;

let%expect_test "enforce_failures" =
  (* This covers cases yielding enforce failures. *)
  let init = {| (executable) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  (* Certain fields don't have heuristics in place for initializing a value if
     it isn't there. *)
  require_does_raise (fun () -> test [ has_field `name ]);
  [%expect
    {| (Dunolinter.Handler.Enforce_failure (loc _) (condition (has_field name))) |}];
  require_does_raise (fun () -> test [ has_field `public_name ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (has_field public_name)))
    |}];
  let init = {| (executable (name my_exe)) |} in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  let test_fails cond =
    let t = parse init in
    require_does_raise (fun () -> enforce t cond)
  in
  test [ public_name (equals (Dune.Executable.Public_name.v "public-main")) ];
  [%expect
    {|
    -1,2 +1,3
      (executable
    -| (name my_exe))
    +| (name my_exe)
    +| (public_name public-main))
    |}];
  (* [is_prefix] and [is_suffix] cannot provide initial values - enforcement fails. *)
  test_fails [ public_name (is_prefix "prefix_") ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (public_name (is_prefix prefix_))))
    |}];
  test_fails [ public_name (is_suffix "_suffix") ];
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (public_name (is_suffix _suffix))))
    |}];
  ()
;;

let%expect_test "undefined conditions" =
  let init = {| (executable (public_name my-cli)) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  let main = Dune.Executable.Name.v "main" in
  let _, t = parse init in
  (* Evaluation of [is_prefix] on an absent field is undefined. *)
  Test_helpers.is_undefined
    (Dune_linter.Executable.eval t ~predicate:(`name (is_prefix "hey")));
  [%expect {||}];
  (* When a condition is undefined, the entire if-then-else is ignored. *)
  test [ if_ (name (is_prefix "hey")) (name (equals main)) (name (is_suffix "ho")) ];
  [%expect {| (executable (public_name my-cli)) |}];
  (* Beware of static code simplifications performed by Blang though! In the
     following example, the [if_] is rewritten as a [And _] sequence. Since
     [is_prefix] is now at a positive enforcing position but cannot provide
     an initial value for the absent field, enforcement fails. *)
  require_does_raise (fun () ->
    test [ if_ (name (is_prefix "hey")) (name (equals main)) false_ ]);
  [%expect
    {|
    (Dunolinter.Handler.Enforce_failure (loc _)
     (condition (name (is_prefix hey))))
    |}];
  ()
;;

let%expect_test "non base negations" =
  let init = {| (executable (public_name my-cli)) |} in
  let test cond =
    let t = parse init in
    enforce t cond
  in
  test [ not_ (name (or_ [ is_prefix "hey"; is_suffix "ho" ])) ];
  [%expect {| (executable (public_name my-cli)) |}];
  test [ not_ (or_ [ name (is_prefix "hey"); name (is_suffix "ho") ]) ];
  [%expect {| (executable (public_name my-cli)) |}];
  ()
;;

let%expect_test "has_field_auto_initialize" =
  (* Test fields that can be auto-initialized when missing. *)
  let init = {| (executable (name my-exe)) |} in
  (* [instrumentation] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `instrumentation ];
  [%expect {| (executable (name my-exe) (instrumentation (backend bisect_ppx))) |}];
  (* [lint] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `lint ];
  [%expect {| (executable (name my-exe) (lint (pps))) |}];
  (* [preprocess] field can be auto-initialized. *)
  let t = parse init in
  enforce t [ has_field `preprocess ];
  [%expect {| (executable (name my-exe) (preprocess no_preprocessing)) |}];
  ()
;;

let%expect_test "field_conditions" =
  (* Test field-specific condition enforcement .*)
  let init = {| (executable (name my-exe)) |} in
  (* [instrumentation] condition auto-creates field. *)
  let t = parse init in
  enforce
    t
    [ instrumentation (backend (Dune.Instrumentation.Backend.Name.v "bisect_ppx")) ];
  [%expect {| (executable (name my-exe) (instrumentation (backend bisect_ppx))) |}];
  (* [lint] condition auto-creates field. *)
  let t = parse init in
  enforce t [ lint (pps (pp (Dune.Pp.Name.v "ppx_linter"))) ];
  [%expect {| (executable (name my-exe) (lint (pps ppx_linter))) |}];
  (* [preprocess] condition auto-creates field. *)
  let t = parse init in
  enforce t [ preprocess no_preprocessing ];
  [%expect {| (executable (name my-exe) (preprocess no_preprocessing)) |}];
  ()
;;

let%expect_test "remove_fields" =
  (* Test removing fields via [not has_field]. *)
  let init =
    {|
(executable
 (name my-exe)
 (public_name my-cli)
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
    -1,8 +1,6
      (executable
       (name my-exe)
       (public_name my-cli)
    -| (instrumentation
    -|  (backend bisect_ppx))
       (lint
        (pps ppx_linter))
       (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `lint) ];
  [%expect
    {|
    -3,6 +3,4
       (public_name my-cli)
       (instrumentation
        (backend bisect_ppx))
    -| (lint
    -|  (pps ppx_linter))
       (preprocess no_preprocessing))
    |}];
  test [ not_ (has_field `preprocess) ];
  [%expect
    {|
    -4,5 +4,4
       (instrumentation
        (backend bisect_ppx))
       (lint
    -|  (pps ppx_linter))
    -| (preprocess no_preprocessing))
    +|  (pps ppx_linter)))
    |}];
  test [ not_ (has_field `name) ];
  [%expect
    {|
    -1,5 +1,4
      (executable
    -| (name my-exe)
       (public_name my-cli)
       (instrumentation
        (backend bisect_ppx))
    |}];
  test [ not_ (has_field `public_name) ];
  [%expect
    {|
    -1,6 +1,5
      (executable
       (name my-exe)
    -| (public_name my-cli)
       (instrumentation
        (backend bisect_ppx))
       (lint
    |}];
  ()
;;

let%expect_test "positive_enforcement_with_existing_fields" =
  (* Test enforcement conditions when fields are already present. *)
  let init =
    {|
(executable
 (name my-exe)
 (public_name my-cli)
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
(executable
 (name my-exe)
 (public_name my-cli)
 (instrumentation (backend bisect_ppx))
 (lint (pps ppx_linter))
 (preprocess no_preprocessing))
|}
  in
  let test cond =
    let t = parse init in
    enforce_diff t cond
  in
  test [ public_name (equals (Dune.Executable.Public_name.v "new-name")) ];
  [%expect
    {|
    -1,6 +1,6
      (executable
       (name my-exe)
    -| (public_name my-cli)
    +| (public_name new-name)
       (instrumentation
        (backend bisect_ppx))
       (lint
    |}];
  test [ public_name (is_prefix "cli-"); public_name (is_suffix "-pub") ];
  [%expect
    {|
    -1,6 +1,6
      (executable
       (name my-exe)
    -| (public_name my-cli)
    +| (public_name cli-my-cli-pub)
       (instrumentation
        (backend bisect_ppx))
       (lint
    |}];
  test [ instrumentation (backend (Dune.Instrumentation.Backend.Name.v "coverage")) ];
  [%expect
    {|
    -2,7 +2,7
       (name my-exe)
       (public_name my-cli)
       (instrumentation
    -|  (backend bisect_ppx))
    +|  (backend coverage))
       (lint
        (pps ppx_linter))
       (preprocess no_preprocessing))
    |}];
  test [ lint (pps (pp (Dune.Pp.Name.v "ppx_deriving"))) ];
  [%expect
    {|
    -4,5 +4,5
       (instrumentation
        (backend bisect_ppx))
       (lint
    -|  (pps ppx_linter))
    +|  (pps ppx_deriving ppx_linter))
       (preprocess no_preprocessing))
    |}];
  test [ preprocess (pps (pp (Dune.Pp.Name.v "ppx_compare"))) ];
  [%expect
    {|
    -5,4 +5,5
        (backend bisect_ppx))
       (lint
        (pps ppx_linter))
    -| (preprocess no_preprocessing))
    +| (preprocess
    +|  (pps ppx_compare)))
    |}];
  ()
;;
