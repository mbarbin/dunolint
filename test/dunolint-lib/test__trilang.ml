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

module Trilang = Dunolint.Trilang

let%expect_test "all" =
  List.iter Trilang.all ~f:(fun t -> print_s [%sexp (t : Trilang.t)]);
  [%expect
    {|
    True
    False
    Undefined
    |}];
  ()
;;

let%expect_test "const" =
  let t = Trilang.const true in
  require_equal [%here] (module Trilang) t True;
  let t = Trilang.const false in
  require_equal [%here] (module Trilang) t False;
  ()
;;

let%expect_test "eval" =
  let test a = print_s [%sexp (Trilang.eval a ~f:Fn.id : Trilang.t)] in
  test Blang.true_;
  [%expect {| True |}];
  test Blang.false_;
  [%expect {| False |}];
  test (Blang.base Trilang.True);
  [%expect {| True |}];
  test (Blang.base Trilang.False);
  [%expect {| False |}];
  test (Blang.base Trilang.Undefined);
  [%expect {| Undefined |}];
  let and_table =
    let open List.Let_syntax in
    let%bind a = Trilang.all in
    let%bind b = Trilang.all in
    [ a, b, Blang.and_ [ Blang.base a; Blang.base b ] ]
  in
  List.iter and_table ~f:(fun (a, b, expr) ->
    let result = Trilang.eval expr ~f:Fn.id in
    let and_ = Trilang.Private.and_ a b in
    require_equal [%here] (module Trilang) result and_;
    print_s [%sexp { expr : Trilang.t Blang.t; eval = (result : Trilang.t) }]);
  [%expect
    {|
    ((expr (and True True)) (eval True))
    ((expr (and True False)) (eval False))
    ((expr (and True Undefined)) (eval Undefined))
    ((expr (and False True)) (eval False))
    ((expr (and False False)) (eval False))
    ((expr (and False Undefined)) (eval False))
    ((expr (and Undefined True)) (eval Undefined))
    ((expr (and Undefined False)) (eval False))
    ((expr (and Undefined Undefined)) (eval Undefined))
    |}];
  let or_table =
    let open List.Let_syntax in
    let%bind a = Trilang.all in
    let%bind b = Trilang.all in
    [ a, b, Blang.or_ [ Blang.base a; Blang.base b ] ]
  in
  List.iter or_table ~f:(fun (a, b, expr) ->
    let result = Trilang.eval expr ~f:Fn.id in
    let or_ = Trilang.Private.or_ a b in
    require_equal [%here] (module Trilang) result or_;
    print_s [%sexp { expr : Trilang.t Blang.t; eval = (result : Trilang.t) }]);
  [%expect
    {|
    ((expr (or True True)) (eval True))
    ((expr (or True False)) (eval True))
    ((expr (or True Undefined)) (eval True))
    ((expr (or False True)) (eval True))
    ((expr (or False False)) (eval False))
    ((expr (or False Undefined)) (eval Undefined))
    ((expr (or Undefined True)) (eval True))
    ((expr (or Undefined False)) (eval Undefined))
    ((expr (or Undefined Undefined)) (eval Undefined))
    |}];
  let not_table =
    let open List.Let_syntax in
    let%bind a = Trilang.all in
    [ Blang.not_ (Blang.base a) ]
  in
  List.iter not_table ~f:(fun expr ->
    print_s
      [%sexp
        { expr : Trilang.t Blang.t; eval = (Trilang.eval expr ~f:Fn.id : Trilang.t) }]);
  [%expect
    {|
    ((expr (not True)) (eval False))
    ((expr (not False)) (eval True))
    ((expr (not Undefined)) (eval Undefined))
    |}];
  let if_table =
    let open List.Let_syntax in
    let%bind a = Trilang.all in
    [ Blang.if_ (Blang.base a) (Blang.base Trilang.True) (Blang.base Trilang.False) ]
  in
  List.iter if_table ~f:(fun expr ->
    print_s
      [%sexp
        { expr : Trilang.t Blang.t; eval = (Trilang.eval expr ~f:Fn.id : Trilang.t) }]);
  [%expect
    {|
    ((expr (if True True False)) (eval True))
    ((expr (if False True False)) (eval False))
    ((expr (if Undefined True False)) (eval Undefined))
    |}];
  ()
;;

let%expect_test "disjunction" =
  let test ts = print_s [%sexp (Trilang.disjunction ts : Trilang.t)] in
  test [];
  [%expect {| False |}];
  test [ True ];
  [%expect {| True |}];
  test [ True; Undefined ];
  [%expect {| True |}];
  test [ False ];
  [%expect {| False |}];
  test [ False; Undefined ];
  [%expect {| Undefined |}];
  test [ Undefined; True ];
  [%expect {| True |}];
  test [ Undefined; False ];
  [%expect {| Undefined |}];
  ()
;;

let%expect_test "conjunction" =
  let test ts = print_s [%sexp (Trilang.conjunction ts : Trilang.t)] in
  test [];
  [%expect {| True |}];
  test [ True ];
  [%expect {| True |}];
  test [ True; Undefined ];
  [%expect {| Undefined |}];
  test [ False ];
  [%expect {| False |}];
  test [ False; Undefined ];
  [%expect {| False |}];
  test [ Undefined; True ];
  [%expect {| Undefined |}];
  test [ Undefined; False ];
  [%expect {| False |}];
  ()
;;
