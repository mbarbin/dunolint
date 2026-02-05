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
