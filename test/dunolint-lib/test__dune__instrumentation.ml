(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
    | exception e -> print_s (e |> Exn.sexp_of_t)
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
    | exception e -> print_s (e |> Exn.sexp_of_t)
  in
  (* Backend with no name (empty fields). *)
  test "(backend)";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [backend] expects one or more arguments."
       (suggestion "Replace by: (backend ARG...)")))
     (invalid_sexp (backend)))
    |}];
  (* Backend starting with a list instead of an atom. *)
  test "(backend (nested thing))";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [backend] expects the name of a backend first."
       (suggestion "Replace by: (backend NAME FLAG...)")))
     (invalid_sexp (nested thing)))
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
  (* [present] and [absent] *)
  let present_a : Dune.Instrumentation.Predicate.t =
    `present [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ]
  in
  require (Dune.Instrumentation.Predicate.equal present_a present_a);
  require
    (Dune.Instrumentation.Predicate.equal
       present_a
       (`present [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ]));
  require
    (not
       (Dune.Instrumentation.Predicate.equal
          present_a
          (`present [ Dune.Instrumentation.Backend.Name.v "landmarks" ])));
  require
    (not
       (Dune.Instrumentation.Predicate.equal
          present_a
          (`absent [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ])));
  require
    (Dune.Instrumentation.Predicate.equal
       (`absent [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ])
       (`absent [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ]));
  require
    (not
       (Dune.Instrumentation.Predicate.equal
          (`absent [ Dune.Instrumentation.Backend.Name.v "bisect_ppx" ])
          present_a));
  require
    (not
       (Dune.Instrumentation.Predicate.equal
          present_a
          (`backend (Dune.Instrumentation.Backend.v "bisect_ppx"))));
  require
    (not
       (Dune.Instrumentation.Predicate.equal
          (`backend (Dune.Instrumentation.Backend.v "bisect_ppx"))
          present_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "of_string" =
  let test str =
    match Dune.Instrumentation.Backend.Name.of_string str with
    | Ok name ->
      print_s (List [ Atom "Ok"; Dune.Instrumentation.Backend.Name.sexp_of_t name ])
    | Error (`Msg msg) -> print_s (List [ Atom "Error"; List [ Atom "Msg"; Atom msg ] ])
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
  test
    (present
       [ Dune.Instrumentation.Backend.Name.v "bisect_ppx"
       ; Dune.Instrumentation.Backend.Name.v "landmarks"
       ]);
  [%expect {| (present bisect_ppx landmarks) |}];
  test (absent [ Dune.Instrumentation.Backend.Name.v "landmarks" ]);
  [%expect {| (absent landmarks) |}];
  ()
;;
