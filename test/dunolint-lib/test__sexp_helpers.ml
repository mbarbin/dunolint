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

module Sexp_helpers = Dunolint.Private.Sexp_helpers

(* A simple test type to exercise parse_variant. *)
type test_predicate =
  [ `foo of string
  | `bar of int
  | `nullary
  | `ctx of string
  | `variadic of string list
  ]
[@@deriving sexp_of]

let test_variant_spec : test_predicate Sexp_helpers.Variant_spec.t =
  [ { atom = "foo"; conv = Unary (fun sexp -> `foo (string_of_sexp sexp)) }
  ; { atom = "bar"; conv = Unary (fun sexp -> `bar (int_of_sexp sexp)) }
  ; { atom = "nullary"; conv = Nullary `nullary }
  ; { atom = "ctx"
    ; conv = Unary_with_context (fun ~context:_ ~arg -> `ctx (string_of_sexp arg))
    }
  ; { atom = "variadic"
    ; conv =
        Variadic (fun ~context:_ ~fields -> `variadic (List.map fields ~f:string_of_sexp))
    }
  ]
;;

let%expect_test "parse_variant" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match
      Sexp_helpers.parse_variant test_variant_spec ~error_source:"test_predicate" sexp
    with
    | predicate -> print_s [%sexp (predicate : test_predicate)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  (* Success cases. *)
  test "(foo hello)";
  [%expect {| (foo hello) |}];
  test "(bar 42)";
  [%expect {| (bar 42) |}];
  (* Error: atom that matches a predicate (should take args). *)
  test "foo";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp foo))
    |}];
  test "bar";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp bar))
    |}];
  (* Error: atom that doesn't match any predicate. *)
  test "unknown";
  [%expect
    {|
    (Of_sexp_error "test_predicate_of_sexp: no matching variant found"
     (invalid_sexp unknown))
    |}];
  (* Error: list with atom that doesn't match. *)
  test "(unknown hello)";
  [%expect
    {|
    (Of_sexp_error "test_predicate_of_sexp: no matching variant found"
     (invalid_sexp (unknown hello)))
    |}];
  (* Error: list with wrong number of arguments (zero). *)
  test "(foo)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag \"foo\" has incorrect number of arguments"
     (invalid_sexp (foo)))
    |}];
  (* Error: list with wrong number of arguments (too many). *)
  test "(foo hello world)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag \"foo\" has incorrect number of arguments"
     (invalid_sexp (foo hello world)))
    |}];
  test "(bar 1 2 3)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag \"bar\" has incorrect number of arguments"
     (invalid_sexp (bar 1 2 3)))
    |}];
  (* Error: nested list. *)
  test "((nested) arg)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: a nested list is an invalid polymorphic variant"
     (invalid_sexp ((nested) arg)))
    |}];
  (* Error: empty list. *)
  test "()";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: the empty list is an invalid polymorphic variant"
     (invalid_sexp ()))
    |}];
  (* Success: nullary variant. *)
  test "nullary";
  [%expect {| nullary |}];
  (* Error: nullary variant supplied with argument. *)
  test "(nullary arg)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant does not take arguments"
     (invalid_sexp (nullary arg)))
    |}];
  (* Success: unary_with_context variant. *)
  test "(ctx hello)";
  [%expect {| (ctx hello) |}];
  (* Error: unary_with_context variant with incorrect number of arguments (2). *)
  test "(ctx hello world)";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag \"ctx\" has incorrect number of arguments"
     (invalid_sexp (ctx hello world)))
    |}];
  (* Error: unary_with_context variant without argument. *)
  test "ctx";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp ctx))
    |}];
  (* Success: variadic variant with arguments. *)
  test "(variadic a b c)";
  [%expect {| (variadic (a b c)) |}];
  (* Success: variadic variant with no arguments. *)
  test "(variadic)";
  [%expect {| (variadic ()) |}];
  (* Error: variadic variant without argument (as atom). *)
  test "variadic";
  [%expect
    {|
    (Of_sexp_error
     "test_predicate_of_sexp: polymorphic variant tag takes an argument"
     (invalid_sexp variadic))
    |}];
  ()
;;

(* A simple record type to exercise parse_inline_record. *)
module Test_record = struct
  type t =
    { name : string
    ; value : int
    }

  let t_of_sexp sexp =
    (let error_source = "test_record.t" in
     Sexplib0.Sexp_conv_record.record_of_sexp
       ~caller:error_source
       ~fields:
         (Field
            { name = "name"
            ; kind = Required
            ; conv = string_of_sexp
            ; rest =
                Field
                  { name = "value"; kind = Required; conv = int_of_sexp; rest = Empty }
            })
       ~index_of_field:(function
         | "name" -> 0
         | "value" -> 1
         | _ -> -1)
       ~allow_extra_fields:false
       ~create:(fun (name, (value, ())) -> ({ name; value } : t))
       sexp)
    [@coverage off]
  ;;

  let sexp_of_t { name; value } : Sexp.t =
    List
      [ List [ Atom "name"; sexp_of_string name ]
      ; List [ Atom "value"; sexp_of_int value ]
      ]
  ;;
end

let%expect_test "parse_inline_record" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    let tag, fields =
      match sexp with
      | List (Atom tag :: fields) -> tag, fields
      | _ -> assert false
    in
    match
      Sexp_helpers.parse_inline_record
        (module Test_record)
        ~error_source:"test_record"
        ~context:sexp
        ~tag
        ~fields
    with
    | record -> print_s [%sexp (record : Test_record.t)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  (* Success case: inline record fields (no extra parens). *)
  test "(cons (name hello) (value 42))";
  [%expect {| ((name hello) (value 42)) |}];
  (* Success case: single field. *)
  test "(cons (name foo) (value 1))";
  [%expect {| ((name foo) (value 1)) |}];
  (* Error: missing field. *)
  test "(cons (name hello))";
  [%expect
    {|
    (Of_sexp_error
     "test_record.t_of_sexp: the following record elements were undefined: value"
     (invalid_sexp (cons (name hello))))
    |}];
  (* Error: unknown field. *)
  test "(cons (name hello) (value 42) (extra field))";
  [%expect
    {|
    (Of_sexp_error "test_record.t_of_sexp: extra fields: extra"
     (invalid_sexp (cons (name hello) (value 42) (extra field))))
    |}];
  (* Error: wrong field type. *)
  test "(cons (name hello) (value not_an_int))";
  [%expect
    {|
    (Of_sexp_error "int_of_sexp: (Failure int_of_string)"
     (invalid_sexp (cons (name hello) (value not_an_int))))
    |}];
  ()
;;
