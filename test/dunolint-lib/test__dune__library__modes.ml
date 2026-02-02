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
  let equal = Dune.Library.Modes.Predicate.equal in
  let mem_a = `mem [ `byte; `native ] in
  let mem_b = `mem [ `byte ] in
  let has_mode_byte = `has_mode `byte in
  let has_mode_native = `has_mode `native in
  let has_modes_a = `has_modes [ `byte; `native ] in
  let has_modes_b = `has_modes [ `byte ] in
  (* Physical equality. *)
  require (equal mem_a mem_a);
  [%expect {||}];
  require (equal has_mode_byte has_mode_byte);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`mem [ `byte; `native ]) (`mem [ `byte; `native ]));
  [%expect {||}];
  require (equal (`has_mode `byte) (`has_mode `byte));
  [%expect {||}];
  require (equal (`has_modes [ `byte; `native ]) (`has_modes [ `byte; `native ]));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal mem_a mem_b));
  [%expect {||}];
  require (not (equal has_mode_byte has_mode_native));
  [%expect {||}];
  require (not (equal has_modes_a has_modes_b));
  [%expect {||}];
  (* Test each variant as first argument to cover the catch-all. *)
  require (not (equal mem_a has_mode_byte));
  [%expect {||}];
  require (not (equal mem_a has_modes_a));
  [%expect {||}];
  require (not (equal has_mode_byte mem_a));
  [%expect {||}];
  require (not (equal has_mode_byte has_modes_a));
  [%expect {||}];
  require (not (equal has_modes_a mem_a));
  [%expect {||}];
  require (not (equal has_modes_a has_mode_byte));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Library.Modes.Predicate) p in
  test (mem []);
  [%expect {| (mem) |}];
  test (mem [ `best ]);
  [%expect {| (mem best) |}];
  test (mem [ `byte; `native; `melange ]);
  [%expect {| (mem byte native melange) |}];
  test (mem [ `byte ]);
  [%expect {| (mem byte) |}];
  test (mem [ `native ]);
  [%expect {| (mem native) |}];
  test (mem [ `melange ]);
  [%expect {| (mem melange) |}];
  ()
;;

(* Deprecated EDSL helpers - test serialization only (no roundtrip, since
   parsing normalizes to [mem]). *)
let%expect_test "predicate - deprecated EDSL helpers" =
  let test p = print_s [%sexp (p : Dune.Library.Modes.Predicate.t Blang.t)] in
  test ((has_modes [@alert "-deprecated"]) []);
  [%expect {| (has_modes ()) |}];
  test ((has_modes [@alert "-deprecated"]) [ `best ]);
  [%expect {| (has_modes (best)) |}];
  test ((has_modes [@alert "-deprecated"]) [ `byte; `native; `melange ]);
  [%expect {| (has_modes (byte native melange)) |}];
  test ((has_mode [@alert "-deprecated"]) `byte);
  [%expect {| (has_mode byte) |}];
  test ((has_mode [@alert "-deprecated"]) `native);
  [%expect {| (has_mode native) |}];
  test ((has_mode [@alert "-deprecated"]) `melange);
  [%expect {| (has_mode melange) |}];
  ()
;;

let%expect_test "Predicate.t_of_sexp" =
  let test str =
    let sexp = Parsexp.Single.parse_string_exn str in
    match Dune.Library.Modes.Predicate.t_of_sexp sexp with
    | predicate -> print_s [%sexp (predicate : Dune.Library.Modes.Predicate.t)]
    | exception exn -> print_s [%sexp (exn : Exn.t)]
  in
  test "(mem byte)";
  [%expect {| (mem byte) |}];
  test "(mem byte native)";
  [%expect {| (mem byte native) |}];
  test "(mem)";
  [%expect {| (mem) |}];
  test "mem";
  [%expect
    {|
    (Of_sexp_error
     (Dunolint.Sexp_helpers.Error_context.E
      ("The construct [mem] expects one or more arguments."
       (suggestion "Replace by: (mem ARG)")))
     (invalid_sexp mem))
    |}];
  (* Deprecated operators - parsed and normalized to [mem]. *)
  test "(has_mode byte)";
  [%expect {| (mem byte) |}];
  test "(has_modes (byte native))";
  [%expect {| (mem byte native) |}];
  test "(has_modes ())";
  [%expect {| (mem) |}];
  ()
;;
