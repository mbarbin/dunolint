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

let%expect_test "Param.equal" =
  let equal = Dune.Pps.Predicate.Param.equal in
  let equals_a = `equals "value_a" in
  let equals_b = `equals "value_b" in
  (* Physical equality - only for variant with argument. *)
  require (equal equals_a equals_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal `any `any);
  [%expect {||}];
  require (equal `none `none);
  [%expect {||}];
  require (equal `some `some);
  [%expect {||}];
  require (equal (`equals "value_a") (`equals "value_a"));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal equals_a equals_b));
  [%expect {||}];
  (* Different variants. *)
  require (not (equal `any `none));
  [%expect {||}];
  require (not (equal `none `some));
  [%expect {||}];
  require (not (equal `some equals_a));
  [%expect {||}];
  require (not (equal equals_a `any));
  [%expect {||}];
  ()
;;

let%expect_test "Applies_to.equal" =
  let equal = Dune.Pps.Predicate.Flag.Applies_to.equal in
  let pp_a = `pp (Dune.Pp.Name.v "ppx_a") in
  let pp_b = `pp (Dune.Pp.Name.v "ppx_b") in
  (* Physical equality - only for variant with argument. *)
  require (equal pp_a pp_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal `any `any);
  [%expect {||}];
  require (equal `driver `driver);
  [%expect {||}];
  require (equal (`pp (Dune.Pp.Name.v "ppx_a")) (`pp (Dune.Pp.Name.v "ppx_a")));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal pp_a pp_b));
  [%expect {||}];
  (* Different variants. *)
  require (not (equal `any `driver));
  [%expect {||}];
  require (not (equal `driver pp_a));
  [%expect {||}];
  require (not (equal pp_a `any));
  [%expect {||}];
  ()
;;

let%expect_test "Flag.equal" =
  let equal = Dune.Pps.Predicate.Flag.equal in
  let flag_a : Dune.Pps.Predicate.Flag.t =
    { name = "-a"; param = `any; applies_to = `any }
  in
  let flag_b : Dune.Pps.Predicate.Flag.t =
    { name = "-b"; param = `any; applies_to = `any }
  in
  (* Physical equality. *)
  require (equal flag_a flag_a);
  [%expect {||}];
  (* Structural equality - same value. *)
  require
    (equal
       { name = "-a"; param = `any; applies_to = `any }
       { name = "-a"; param = `any; applies_to = `any });
  [%expect {||}];
  (* Different values. *)
  require (not (equal flag_a flag_b));
  [%expect {||}];
  ()
;;

let%expect_test "Pp_with_flag.equal" =
  let equal = Dune.Pps.Predicate.Pp_with_flag.equal in
  let pwf_a : Dune.Pps.Predicate.Pp_with_flag.t =
    { pp = Dune.Pp.Name.v "ppx_a"; flag = "-f"; param = `any }
  in
  let pwf_b : Dune.Pps.Predicate.Pp_with_flag.t =
    { pp = Dune.Pp.Name.v "ppx_b"; flag = "-f"; param = `any }
  in
  (* Physical equality. *)
  require (equal pwf_a pwf_a);
  [%expect {||}];
  (* Structural equality - same value. *)
  require
    (equal
       { pp = Dune.Pp.Name.v "ppx_a"; flag = "-f"; param = `any }
       { pp = Dune.Pp.Name.v "ppx_a"; flag = "-f"; param = `any });
  [%expect {||}];
  (* Different values. *)
  require (not (equal pwf_a pwf_b));
  [%expect {||}];
  ()
;;

let%expect_test "Predicate.equal" =
  let equal = Dune.Pps.Predicate.equal in
  let pp_a = `pp (Dune.Pp.Name.v "ppx_a") in
  let pp_b = `pp (Dune.Pp.Name.v "ppx_b") in
  let flag_a : Dune.Pps.Predicate.t =
    `flag { Dune.Pps.Predicate.Flag.name = "-a"; param = `any; applies_to = `any }
  in
  let pp_with_flag_a : Dune.Pps.Predicate.t =
    `pp_with_flag
      { Dune.Pps.Predicate.Pp_with_flag.pp = Dune.Pp.Name.v "ppx_a"
      ; flag = "-f"
      ; param = `any
      }
  in
  (* Physical equality. *)
  require (equal pp_a pp_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require (equal (`pp (Dune.Pp.Name.v "ppx_a")) (`pp (Dune.Pp.Name.v "ppx_a")));
  [%expect {||}];
  require
    (equal
       (`flag { Dune.Pps.Predicate.Flag.name = "-a"; param = `any; applies_to = `any })
       (`flag { Dune.Pps.Predicate.Flag.name = "-a"; param = `any; applies_to = `any }));
  [%expect {||}];
  require
    (equal
       (`pp_with_flag
           { Dune.Pps.Predicate.Pp_with_flag.pp = Dune.Pp.Name.v "ppx_a"
           ; flag = "-f"
           ; param = `any
           })
       (`pp_with_flag
           { Dune.Pps.Predicate.Pp_with_flag.pp = Dune.Pp.Name.v "ppx_a"
           ; flag = "-f"
           ; param = `any
           }));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal pp_a pp_b));
  [%expect {||}];
  (* Different variants. *)
  require (not (equal pp_a flag_a));
  [%expect {||}];
  require (not (equal flag_a pp_with_flag_a));
  [%expect {||}];
  require (not (equal pp_with_flag_a pp_a));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "predicate" =
  let test p = Common.test_predicate (module Dune.Pps.Predicate) p in
  test (pp (Dune.Pp.Name.v "ppx_compare"));
  [%expect {| (pp ppx_compare) |}];
  test (flag { name = "-a"; param = `any; applies_to = `any });
  [%expect {| (flag (name -a) (param any) (applies_to any)) |}];
  test (flag { name = "-a"; param = `none; applies_to = `driver });
  [%expect {| (flag (name -a) (param none) (applies_to driver)) |}];
  test
    (flag
       { name = "-a"; param = `some; applies_to = `pp (Dune.Pp.Name.v "ppx_js_style") });
  [%expect {| (flag (name -a) (param some) (applies_to (pp ppx_js_style))) |}];
  test
    (flag
       { name = "-unused-code-warnings"; param = `equals "force"; applies_to = `driver });
  [%expect
    {|
    (flag (name -unused-code-warnings) (param (equals force))
     (applies_to driver))
    |}];
  test
    (pp_with_flag
       { pp = Dune.Pp.Name.v "ppx_js_style"
       ; flag = "-allow-let-operators"
       ; param = `none
       });
  [%expect
    {| (pp_with_flag (pp ppx_js_style) (flag -allow-let-operators) (param none)) |}];
  ()
;;
