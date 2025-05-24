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
    (Dune_linter.Library.eval
       t
       ~predicate:(`modes (equals (Dune.Library.Modes.of_list [ `best ]))));
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
    (Dune_linter.Library.eval
       t
       ~predicate:(`modes (equals (Dune.Library.Modes.of_list [ `byte; `native ]))));
  [%expect {||}];
  Test_helpers.is_false
    (Dune_linter.Library.eval
       t
       ~predicate:(`modes (equals (Dune.Library.Modes.of_list [ `best ]))));
  [%expect {||}];
  ()
;;

let%expect_test "enforce" =
  let _ = (`none : [ `some of Predicate.t | `none ]) in
  let enforce ((sexps_rewriter, field), t) conditions =
    Sexps_rewriter.reset sexps_rewriter;
    Dunolinter.Handler.raise ~f:(fun () ->
      List.iter conditions ~f:(fun condition -> Dune_linter.Library.enforce t ~condition);
      Dune_linter.Library.rewrite t ~sexps_rewriter ~field;
      print_s (Sexps_rewriter.contents sexps_rewriter |> Parsexp.Single.parse_string_exn))
  in
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
  ()
;;
