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

open Dunolint.Std

let%expect_test "Backend.equal" =
  let equal = Dune.Instrumentation.Backend.equal in
  let bisect = Dune.Instrumentation.Backend.v "bisect_ppx" in
  let landmarks = Dune.Instrumentation.Backend.v "landmarks" in
  let windtrap = Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ] in
  let windtrap_no_flags = Dune.Instrumentation.Backend.v "ppx_windtrap" in
  (* Physical equality. *)
  require (equal bisect bisect);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (Dune.Instrumentation.Backend.v "bisect_ppx")
       (Dune.Instrumentation.Backend.v "bisect_ppx"));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal bisect landmarks));
  [%expect {||}];
  (* Backends with flags. *)
  require
    (equal
       windtrap
       (Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]));
  [%expect {||}];
  (* Same name but different flags are not equal. *)
  require (not (equal windtrap windtrap_no_flags));
  [%expect {||}];
  (* Different name, same flags. *)
  require
    (not
       (equal
          (Dune.Instrumentation.Backend.v "backend_a" ~flags:[ "--coverage" ])
          (Dune.Instrumentation.Backend.v "backend_b" ~flags:[ "--coverage" ])));
  [%expect {||}];
  ()
;;

let%expect_test "Backend.sexp roundtrip" =
  let test b =
    let sexp = Dune.Instrumentation.Backend.sexp_of_t b in
    let b' = Dune.Instrumentation.Backend.t_of_sexp sexp in
    require (Dune.Instrumentation.Backend.equal b b');
    print_s sexp
  in
  test (Dune.Instrumentation.Backend.v "bisect_ppx");
  [%expect {| bisect_ppx |}];
  test (Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]);
  [%expect {| (ppx_windtrap --coverage) |}];
  ()
;;

let%expect_test "Backend.t_of_sexp - error cases" =
  let test sexp_str =
    match
      Dune.Instrumentation.Backend.t_of_sexp (Parsexp.Single.parse_string_exn sexp_str)
    with
    | _ -> assert false
    | exception e -> print_s [%sexp (e : exn)]
  in
  (* Empty list. *)
  test "()";
  [%expect {| (Sexplib0__Sexp_conv_error.No_variant_match) |}];
  (* List starting with a list. *)
  test "((nested) arg)";
  [%expect {| (Sexplib0__Sexp_conv_error.No_variant_match) |}];
  ()
;;

let%expect_test "Predicate.t_of_sexp - error cases" =
  let test sexp_str =
    match
      Dune.Instrumentation.Predicate.t_of_sexp (Parsexp.Single.parse_string_exn sexp_str)
    with
    | _ -> assert false
    | exception e -> print_s [%sexp (e : exn)]
  in
  (* Backend with no name (empty fields). *)
  test "(backend)";
  [%expect
    {|
    (Of_sexp_error
     "instrumentation.t_of_sexp: sum tag \"backend\" has incorrect number of arguments"
     (invalid_sexp ()))
    |}];
  (* Backend starting with a list instead of an atom. *)
  test "(backend (nested thing))";
  [%expect
    {|
    (Of_sexp_error
     "instrumentation.t_of_sexp: sum tag \"backend\" has incorrect number of arguments"
     (invalid_sexp ()))
    |}];
  ()
;;

let%expect_test "Predicate.equal" =
  let equal = Dune.Instrumentation.Predicate.equal in
  let backend_a = `backend (Dune.Instrumentation.Backend.v "bisect_ppx") in
  let backend_b = `backend (Dune.Instrumentation.Backend.v "landmarks") in
  let backend_c =
    `backend (Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ])
  in
  let backend_d = `backend (Dune.Instrumentation.Backend.v "ppx_windtrap") in
  (* Physical equality. *)
  require (equal backend_a backend_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (`backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
       (`backend (Dune.Instrumentation.Backend.v "bisect_ppx")));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal backend_a backend_b));
  [%expect {||}];
  (* Backends with flags: same name different flags are not equal. *)
  require (not (equal backend_c backend_d));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp
        (Dune.Instrumentation.Backend.Name.of_string str
         : (Dune.Instrumentation.Backend.Name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid Dunolint.Instrumentation.Backend.Name")) |}];
  test "bisect_ppx";
  [%expect {| (Ok bisect_ppx) |}];
  test "backend-dash";
  [%expect
    {|
    (Error
     (Msg "\"backend-dash\": invalid Dunolint.Instrumentation.Backend.Name"))
    |}];
  test "backend_underscore";
  [%expect {| (Ok backend_underscore) |}];
  test "backend.dot";
  [%expect {| (Ok backend.dot) |}];
  test "backend#sharp";
  [%expect
    {|
    (Error
     (Msg "\"backend#sharp\": invalid Dunolint.Instrumentation.Backend.Name"))
    |}];
  test "backend@at";
  [%expect
    {| (Error (Msg "\"backend@at\": invalid Dunolint.Instrumentation.Backend.Name")) |}];
  ()
;;

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Instrumentation.Predicate) p in
  test (backend (Dune.Instrumentation.Backend.v "bisect_ppx"));
  [%expect {| (backend bisect_ppx) |}];
  test (backend (Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:[ "--coverage" ]));
  [%expect {| (backend ppx_windtrap --coverage) |}];
  ()
;;
