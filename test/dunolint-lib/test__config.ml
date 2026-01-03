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

let%expect_test "Config.equal" =
  let equal = Dunolint.Config.equal in
  let v1_a = Dunolint.Config.v1 (Dunolint.Config.V1.create []) in
  let v1_b =
    Dunolint.Config.v1
      (Dunolint.Config.V1.create [ `skip_paths [ Dunolint.Glob.v ".git/" ] ])
  in
  (* Physical equality. *)
  require (equal v1_a v1_a);
  [%expect {||}];
  (* Structural equality - same variant, same value. *)
  require
    (equal
       (Dunolint.Config.v1 (Dunolint.Config.V1.create []))
       (Dunolint.Config.v1 (Dunolint.Config.V1.create [])));
  [%expect {||}];
  (* Same variant, different value. *)
  require (not (equal v1_a v1_b));
  [%expect {||}];
  ()
;;

let%expect_test "Config.V1.equal" =
  let equal = Dunolint.Config.V1.equal in
  let get_v1 t =
    match Dunolint.Config.Private.view t with
    | `v1 v1 -> v1
  in
  let v1_a = get_v1 (Dunolint.Config.v1 (Dunolint.Config.V1.create [])) in
  let v1_b =
    get_v1
      (Dunolint.Config.v1
         (Dunolint.Config.V1.create [ `skip_paths [ Dunolint.Glob.v ".git/" ] ]))
  in
  let v1_c =
    get_v1
      (Dunolint.Config.v1
         (Dunolint.Config.V1.create [ `skip_paths [ Dunolint.Glob.v "node_modules/" ] ]))
  in
  (* Physical equality. *)
  require (equal v1_a v1_a);
  [%expect {||}];
  (* Structural equality - same value. *)
  require (equal (Dunolint.Config.V1.create []) (Dunolint.Config.V1.create []));
  [%expect {||}];
  require
    (equal
       (Dunolint.Config.V1.create [ `skip_paths [ Dunolint.Glob.v ".git/" ] ])
       (Dunolint.Config.V1.create [ `skip_paths [ Dunolint.Glob.v ".git/" ] ]));
  [%expect {||}];
  (* Different values. *)
  require (not (equal v1_a v1_b));
  [%expect {||}];
  require (not (equal v1_b v1_c));
  [%expect {||}];
  ()
;;

let%expect_test "Config.V1.equal - with rules" =
  let equal = Dunolint.Config.V1.equal in
  let rule_a, rule_b =
    let open Dunolint.Config.Std in
    enforce (dune (has_field `instrumentation)), enforce (dune (has_field `lint))
  in
  let v1_no_rule = Dunolint.Config.V1.create [] in
  let v1_rule_a = Dunolint.Config.V1.create [ `rule rule_a ] in
  let v1_rule_b = Dunolint.Config.V1.create [ `rule rule_b ] in
  let v1_two_rules = Dunolint.Config.V1.create [ `rule rule_a; `rule rule_b ] in
  (* Physical equality. *)
  require (equal v1_rule_a v1_rule_a);
  [%expect {||}];
  (* Structural equality - same rules. *)
  require
    (equal
       (Dunolint.Config.V1.create [ `rule rule_a ])
       (Dunolint.Config.V1.create [ `rule rule_a ]));
  [%expect {||}];
  let stanza_a = `rule rule_a in
  let stanza_b = `skip_paths [ Dunolint.Glob.v ".git/" ] in
  (* Structural equality with inner phys_equal. *)
  require
    (equal
       (Dunolint.Config.V1.create [ stanza_a ])
       (Dunolint.Config.V1.create [ stanza_a ]));
  [%expect {||}];
  (* Different stanzas. *)
  require
    (not
       (equal
          (Dunolint.Config.V1.create [ stanza_a ])
          (Dunolint.Config.V1.create [ stanza_b ])));
  [%expect {||}];
  require
    (not
       (equal
          (Dunolint.Config.V1.create [ stanza_b ])
          (Dunolint.Config.V1.create [ stanza_a ])));
  [%expect {||}];
  require (not (equal v1_no_rule v1_rule_a));
  [%expect {||}];
  require (not (equal v1_rule_a v1_rule_b));
  [%expect {||}];
  require (not (equal v1_rule_a v1_two_rules));
  [%expect {||}];
  ()
;;

open Dunolint.Config.Std

let%expect_test "create" =
  let t = Dunolint.Config.create () in
  print_s [%sexp (t : Dunolint.Config.t)];
  [%expect {| (stanzas (lang dunolint 1.0)) |}];
  require_equal (module Dunolint.Config) t t;
  [%expect {||}];
  ()
;;

let%expect_test "empty v1" =
  let t = Dunolint.Config.V1.create [] in
  print_s [%sexp (Dunolint.Config.V1.skip_paths t : Dunolint.Glob.t list list)];
  [%expect {| () |}];
  print_s [%sexp (Dunolint.Config.V1.rules t : Dunolint.Config.Rule.t list)];
  [%expect {| () |}];
  let t = Dunolint.Config.v1 t in
  print_s [%sexp (t : Dunolint.Config.t)];
  [%expect {| (stanzas (lang dunolint 1.0)) |}];
  require_equal (module Dunolint.Config) t t;
  [%expect {||}];
  ()
;;

let%expect_test "non-empty-v1" =
  let t =
    Dunolint.Config.V1.create
      [ `skip_paths [ Dunolint.Glob.v ".git/" ]
      ; `rule (enforce (dune (has_field `instrumentation)))
      ]
    |> Dunolint.Config.v1
  in
  let v1 =
    match Dunolint.Config.Private.view t with
    | `v1 v1 -> v1
  in
  print_s [%sexp (Dunolint.Config.V1.rules v1 : Dunolint.Config.Rule.t list)];
  [%expect {| ((enforce (dune (has_field instrumentation)))) |}];
  print_s [%sexp (v1 : Dunolint.Config.V1.t)];
  [%expect
    {|
    ((stanzas
      ((skip_paths .git/) (rule (enforce (dune (has_field instrumentation)))))))
    |}];
  print_s [%sexp (t : Dunolint.Config.t)];
  [%expect
    {|
    (stanzas (lang dunolint 1.0) (skip_paths .git/)
     (rule (enforce (dune (has_field instrumentation)))))
    |}];
  require_equal (module Dunolint.Config) t t;
  [%expect {||}];
  let rules = Dunolint.Config.V1.rules v1 in
  let t' = Dunolint.Config.create ~rules () in
  (* t and t' are different because t includes skip_paths. *)
  print_s [%sexp (Dunolint.Config.equal t t' : bool)];
  [%expect {| false |}];
  ()
;;

let test_roundtrip c =
  let sexps = Dunolint.Config.to_stanzas c in
  let c' = Dunolint.Config.of_stanzas sexps in
  require_equal (module Dunolint.Config) c c';
  List.iter sexps ~f:print_s;
  ()
;;

let%expect_test "versioned_sexp v1" =
  let v1 = Dunolint.Config.V1.create [] in
  let t = Dunolint.Config.v1 v1 in
  test_roundtrip t;
  [%expect {| (lang dunolint 1.0) |}];
  print_endline (Dunolint.Config.to_file_contents t ~generated_by:"test_config.ml");
  [%expect
    {|
    ;; This file is generated by [test_config.ml]. Do not edit!
    (lang dunolint 1.0)
    |}];
  let () =
    match Dunolint.Config.Private.view t with
    | `v1 v1' -> require_equal (module Dunolint.Config.V1) v1 v1'
  in
  [%expect {| |}];
  ()
;;

let%expect_test "unsupported version" =
  let sexp = Sexp.List [ List [ Atom "version"; Atom "unknown" ]; List [] ] in
  require_does_raise (fun () -> (Dunolint.Config.of_stanzas [ sexp ] : Dunolint.Config.t));
  [%expect
    {|
    (Of_sexp_error
     "Dunolint config expected to start with (lang dunolint VERSION)."
     (invalid_sexp ((version unknown) ())))
    |}];
  let sexp = Sexp.List [ Atom "lang"; Atom "dunolint"; Atom "unknown" ] in
  require_does_raise (fun () -> (Dunolint.Config.of_stanzas [ sexp ] : Dunolint.Config.t));
  [%expect
    {|
    (Of_sexp_error "Unsupported dunolint config version [unknown]."
     (invalid_sexp unknown))
    |}];
  ()
;;
