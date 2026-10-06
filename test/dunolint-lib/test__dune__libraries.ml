(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open Dunolint.Std

let%expect_test "Predicate.equal" =
  let equal = Dune.Libraries.Predicate.equal in
  let mem_a = `mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ] in
  let mem_b = `mem [ Dune.Library.Name.v "base" ] in
  let mem_c = `mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ] in
  (* Physical equality. *)
  require (equal mem_a mem_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal mem_a mem_c);
  [%expect {||}];
  require
    (equal (`mem [ Dune.Library.Name.v "base" ]) (`mem [ Dune.Library.Name.v "base" ]));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal mem_a mem_b));
  [%expect {||}];
  (* Empty list. *)
  require (equal (`mem []) (`mem []));
  [%expect {||}];
  (* Other variants. *)
  let base = Dune.Library.Name.v "base" in
  require (equal (`present [ base ]) (`present [ base ]));
  require (equal (`absent [ base ]) (`absent [ base ]));
  require (not (equal (`present [ base ]) (`absent [ base ])));
  require (not (equal (`absent [ base ]) (`mem [ base ])));
  require (not (equal (`mem [ base ]) (`present [ base ])));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Libraries.Predicate) p in
  (* Deprecated. *)
  test ((mem [@alert "-deprecated"]) []);
  [%expect {| (mem) |}];
  test
    ((mem [@alert "-deprecated"])
       [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]);
  [%expect {| (mem base core) |}];
  test (present [ Dune.Library.Name.v "base" ]);
  [%expect {| (present base) |}];
  test
    (present
       [ Dune.Library.Name.v "base"
       ; Dune.Library.Name.v "core"
       ; Dune.Library.Name.v "my-lib.sub-lib"
       ]);
  [%expect {| (present base core my-lib.sub-lib) |}];
  test (present [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]);
  [%expect {| (present base core) |}];
  test (absent [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]);
  [%expect {| (absent base core) |}];
  ()
;;

let%expect_test "Predicate.t_of_sexp" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match Dune.Libraries.Predicate.t_of_sexp sexp with
    | predicate -> print_s (predicate |> Dune.Libraries.Predicate.sexp_of_t)
    | exception exn -> print_s (exn |> Exn.sexp_of_t)
  in
  test "(mem base)";
  [%expect {| (mem base) |}];
  test "(mem base core)";
  [%expect {| (mem base core) |}];
  test "(mem)";
  [%expect {| (mem) |}];
  test "mem";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [mem] must be written within parentheses."
       (suggestion "Replace by: (mem ARG...)")))
     (invalid_sexp mem))
    |}];
  test "(present base)";
  [%expect {| (present base) |}];
  test "(present)";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [present] expects one or more arguments."
       (suggestion "Replace by: (present ARG...)")))
     (invalid_sexp (present)))
    |}];
  test "(absent base)";
  [%expect {| (absent base) |}];
  test "(absent)";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [absent] expects one or more arguments."
       (suggestion "Replace by: (absent ARG...)")))
     (invalid_sexp (absent)))
    |}];
  ()
;;
