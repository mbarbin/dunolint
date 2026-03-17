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
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Libraries.Predicate) p in
  test (mem []);
  [%expect {| (mem) |}];
  test (mem [ Dune.Library.Name.v "base" ]);
  [%expect {| (mem base) |}];
  test (mem [ Dune.Library.Name.v "base"; Dune.Library.Name.v "core" ]);
  [%expect {| (mem base core) |}];
  test
    (mem
       [ Dune.Library.Name.v "base"
       ; Dune.Library.Name.v "core"
       ; Dune.Library.Name.v "my-lib.sub-lib"
       ]);
  [%expect {| (mem base core my-lib.sub-lib) |}];
  ()
;;
